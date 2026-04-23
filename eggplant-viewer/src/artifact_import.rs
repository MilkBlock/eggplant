use crate::{DemoGraph, EventHandle};
use eggplant_egui_graphs::{
    ENode, ENodeDslMetadata, EventHandler, FuncOffset, Graph, InnerPos, MaybeInner, ViewEdge,
    ViewNode,
};
use egraph_serialize::{ClassId, EGraph as SerializedEGraph, NodeId};
use indexmap::IndexMap;
use petgraph::stable_graph::StableGraph;
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Deserialize)]
pub struct ImportedSerializedEggplantArtifact {
    pub dsl_schema: ImportedDslSchemaManifest,
    pub payload: ImportedSerializedGraphPayload,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ImportedSerializedGraphPayload {
    pub egraph: SerializedEGraph,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ImportedDslSchemaManifest {
    #[serde(default)]
    pub variants: Vec<ImportedDslVariantSchema>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ImportedDslVariantSchema {
    pub key: String,
    pub owner_ty: String,
    pub variant_name: String,
    #[serde(default)]
    pub fields: Vec<ImportedDslFieldSchema>,
    pub typst_template: Option<String>,
    pub precedence: u16,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ImportedDslFieldSchema {
    pub name: String,
}

#[derive(Debug, Clone)]
struct ViewerVariantMetadata {
    variant_key: String,
    field_names: Vec<String>,
    typst_template: Option<String>,
    precedence: u16,
}

#[derive(Debug, Clone)]
struct RenderedNodeText {
    text: String,
    precedence: u16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArtifactImportError {
    Parse(String),
    Unsupported(String),
}

impl Display for ArtifactImportError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Parse(err) => write!(f, "failed to parse artifact json: {err}"),
            Self::Unsupported(err) => write!(f, "{err}"),
        }
    }
}

impl std::error::Error for ArtifactImportError {}

pub fn load_artifact_graph_from_str(
    data: &str,
    event_handler: Box<dyn EventHandle>,
) -> Result<DemoGraph, ArtifactImportError> {
    let artifact = serde_json::from_str::<ImportedSerializedEggplantArtifact>(data)
        .map_err(|err| ArtifactImportError::Parse(err.to_string()))?;
    build_demo_graph_from_artifact(&artifact, event_handler)
}

fn build_demo_graph_from_artifact(
    artifact: &ImportedSerializedEggplantArtifact,
    event_handler: Box<dyn EventHandle>,
) -> Result<DemoGraph, ArtifactImportError> {
    let metadata_by_identity = artifact
        .dsl_schema
        .variants
        .iter()
        .map(|variant| {
            (
                (variant.owner_ty.clone(), variant.variant_name.clone()),
                ViewerVariantMetadata {
                    variant_key: variant.key.clone(),
                    field_names: variant
                        .fields
                        .iter()
                        .map(|field| field.name.clone())
                        .collect(),
                    typst_template: variant.typst_template.clone(),
                    precedence: variant.precedence,
                },
            )
        })
        .collect::<BTreeMap<_, _>>();

    let mut rendered_cache = BTreeMap::new();
    let mut graph: crate::PetEGraph = Graph::new(StableGraph::default());
    let mut class_to_graph_idx = BTreeMap::new();
    let mut node_id_to_func_offset = BTreeMap::new();
    let mut node_id_to_cano_value = BTreeMap::new();

    for (class_id, class) in artifact.payload.egraph.classes() {
        let cano_value = parse_cano_value(class_id)?;
        let mut enodes_by_func: IndexMap<String, Vec<ENode>> = IndexMap::new();
        for node_id in &class.nodes {
            let node = &artifact.payload.egraph[node_id];
            let func = node.op.clone();
            let offset = enodes_by_func.get(&func).map_or(0, Vec::len);
            let rendered = render_node_text(
                node_id,
                &artifact.payload.egraph,
                &metadata_by_identity,
                &mut rendered_cache,
                &mut BTreeSet::new(),
            );
            let dsl_metadata =
                lookup_variant_metadata(node, &artifact.payload.egraph, &metadata_by_identity).map(
                    |metadata| ENodeDslMetadata {
                        variant_key: metadata.variant_key.clone(),
                        typst_template: metadata.typst_template.clone(),
                        precedence: metadata.precedence,
                    },
                );
            let enode = ENode {
                func_offset: FuncOffset::new(func.clone(), offset),
                cano_value,
                operands_num: node.children.len(),
                basics: Vec::new(),
                display_label: Some(rendered.text),
                dsl_metadata,
            };
            node_id_to_func_offset.insert(node_id.to_string(), enode.func_offset.clone());
            node_id_to_cano_value.insert(node_id.to_string(), cano_value);
            enodes_by_func.entry(func).or_default().push(enode);
        }

        let view_node = ViewNode::new(
            Some(class_id.to_string()),
            enodes_by_func,
            cano_value,
            EventHandler {
                event_handle: event_handler.dyn_clone(),
            },
        );
        let graph_idx = graph.add_node(view_node);
        class_to_graph_idx.insert(class_id.to_string(), graph_idx);
    }

    for (class_id, class) in artifact.payload.egraph.classes() {
        let start_idx = *class_to_graph_idx.get(class_id.as_ref()).ok_or_else(|| {
            ArtifactImportError::Unsupported(format!("missing class {}", class_id))
        })?;
        for node_id in &class.nodes {
            let node = &artifact.payload.egraph[node_id];
            let start_func_offset = node_id_to_func_offset
                .get(node_id.as_ref())
                .ok_or_else(|| {
                    ArtifactImportError::Unsupported(format!(
                        "missing node anchor metadata for {}",
                        node_id
                    ))
                })?
                .clone();
            let start_cano_value =
                *node_id_to_cano_value.get(node_id.as_ref()).ok_or_else(|| {
                    ArtifactImportError::Unsupported(format!(
                        "missing canonical value mapping for {}",
                        node_id
                    ))
                })?;

            for (operand_idx, child_id) in node.children.iter().enumerate() {
                let child_class = artifact.payload.egraph.nid_to_cid(child_id);
                let end_idx = *class_to_graph_idx
                    .get(child_class.as_ref())
                    .ok_or_else(|| {
                        ArtifactImportError::Unsupported(format!(
                            "missing child class {}",
                            child_class
                        ))
                    })?;
                graph.add_edge(
                    start_idx,
                    end_idx,
                    ViewEdge {
                        identifier: None,
                        start_maybe_inner: MaybeInner::Inner {
                            inner_pos: InnerPos {
                                cano_value: start_cano_value,
                                id: start_func_offset.clone(),
                                operand_idx,
                            },
                        },
                    },
                );
            }
        }
    }

    crate::EGraphApp::distribute_nodes_circle_generic::<petgraph::Directed>(&mut graph);
    Ok(DemoGraph::Directed(graph))
}

fn lookup_variant_metadata<'a>(
    node: &egraph_serialize::Node,
    egraph: &'a SerializedEGraph,
    metadata_by_identity: &'a BTreeMap<(String, String), ViewerVariantMetadata>,
) -> Option<&'a ViewerVariantMetadata> {
    let owner_ty = egraph.class_data.get(&node.eclass)?.typ.as_ref()?;
    metadata_by_identity.get(&(owner_ty.clone(), node.op.clone()))
}

fn render_node_text(
    node_id: &NodeId,
    egraph: &SerializedEGraph,
    metadata_by_identity: &BTreeMap<(String, String), ViewerVariantMetadata>,
    rendered_cache: &mut BTreeMap<String, RenderedNodeText>,
    active_stack: &mut BTreeSet<String>,
) -> RenderedNodeText {
    if let Some(cached) = rendered_cache.get(node_id.as_ref()) {
        return cached.clone();
    }

    let node_key = node_id.to_string();
    if !active_stack.insert(node_key.clone()) {
        return RenderedNodeText {
            text: egraph[node_id].op.clone(),
            precedence: u16::MAX,
        };
    }

    let node = &egraph[node_id];
    let rendered = if let Some(metadata) =
        lookup_variant_metadata(node, egraph, metadata_by_identity)
    {
        if let Some(template) = metadata.typst_template.as_deref() {
            let child_fields = node
                .children
                .iter()
                .map(|child| {
                    render_node_text(
                        child,
                        egraph,
                        metadata_by_identity,
                        rendered_cache,
                        active_stack,
                    )
                })
                .collect::<Vec<_>>();
            if child_fields.len() == metadata.field_names.len() {
                let fields = metadata
                    .field_names
                    .iter()
                    .zip(child_fields.iter())
                    .map(|(field_name, rendered)| {
                        (
                            field_name.as_str(),
                            RenderedTemplateField {
                                text: rendered.text.clone(),
                                precedence: rendered.precedence,
                            },
                        )
                    })
                    .collect::<Vec<_>>();
                RenderedNodeText {
                    text: render_template_with_precedence(template, metadata.precedence, &fields),
                    precedence: metadata.precedence,
                }
            } else {
                fallback_node_text(
                    node,
                    egraph,
                    metadata_by_identity,
                    rendered_cache,
                    active_stack,
                )
            }
        } else {
            fallback_node_text(
                node,
                egraph,
                metadata_by_identity,
                rendered_cache,
                active_stack,
            )
        }
    } else {
        fallback_node_text(
            node,
            egraph,
            metadata_by_identity,
            rendered_cache,
            active_stack,
        )
    };

    active_stack.remove(&node_key);
    rendered_cache.insert(node_key, rendered.clone());
    rendered
}

fn fallback_node_text(
    node: &egraph_serialize::Node,
    egraph: &SerializedEGraph,
    metadata_by_identity: &BTreeMap<(String, String), ViewerVariantMetadata>,
    rendered_cache: &mut BTreeMap<String, RenderedNodeText>,
    active_stack: &mut BTreeSet<String>,
) -> RenderedNodeText {
    if node.children.is_empty() {
        return RenderedNodeText {
            text: node.op.clone(),
            precedence: u16::MAX,
        };
    }

    let rendered_children = node
        .children
        .iter()
        .map(|child| {
            render_node_text(
                child,
                egraph,
                metadata_by_identity,
                rendered_cache,
                active_stack,
            )
        })
        .map(|rendered| rendered.text)
        .collect::<Vec<_>>();
    RenderedNodeText {
        text: format!("{}({})", node.op, rendered_children.join(", ")),
        precedence: u16::MAX,
    }
}

#[derive(Debug, Clone)]
struct RenderedTemplateField {
    text: String,
    precedence: u16,
}

fn render_template_with_precedence(
    template: &str,
    parent_precedence: u16,
    fields: &[(&str, RenderedTemplateField)],
) -> String {
    let chars = template.chars().collect::<Vec<_>>();
    let mut rendered = String::new();
    let mut idx = 0usize;

    while idx < chars.len() {
        match chars[idx] {
            '{' => {
                if chars.get(idx + 1) == Some(&'{') {
                    rendered.push('{');
                    idx += 2;
                    continue;
                }

                let start = idx + 1;
                let mut end = start;
                while end < chars.len() && chars[end] != '}' {
                    end += 1;
                }
                let placeholder = chars[start..end].iter().collect::<String>();
                let field = fields
                    .iter()
                    .find(|(name, _)| *name == placeholder)
                    .unwrap_or_else(|| panic!("missing render field `{placeholder}`"));

                if field.1.precedence < parent_precedence {
                    rendered.push('(');
                    rendered.push_str(&field.1.text);
                    rendered.push(')');
                } else {
                    rendered.push_str(&field.1.text);
                }
                idx = end + 1;
            }
            '}' => {
                if chars.get(idx + 1) == Some(&'}') {
                    rendered.push('}');
                    idx += 2;
                } else {
                    rendered.push('}');
                    idx += 1;
                }
            }
            ch => {
                rendered.push(ch);
                idx += 1;
            }
        }
    }

    rendered
}

fn parse_cano_value(class_id: &ClassId) -> Result<u32, ArtifactImportError> {
    let class_id = class_id.to_string();
    let (_sort, rep) = class_id.split_once('-').ok_or_else(|| {
        ArtifactImportError::Unsupported(format!(
            "class id `{class_id}` is not in <sort>-<rep> form"
        ))
    })?;
    rep.parse::<u32>().map_err(|err| {
        ArtifactImportError::Unsupported(format!(
            "class id `{class_id}` does not end with a u32 representative: {err}"
        ))
    })
}

#[cfg(test)]
mod tests {
    use super::load_artifact_graph_from_str;
    use crate::{DemoGraph, EmptyH};
    use eggplant_egui_graphs::EventHandle;

    #[test]
    fn artifact_import_renders_typst_template_labels_from_dsl_metadata() {
        let data = include_str!("../assets/dsl_metadata_demo.json");
        let graph = load_artifact_graph_from_str(data, EmptyH {}.dyn_clone()).unwrap();
        let DemoGraph::Directed(graph) = graph;
        let payloads = graph
            .g()
            .node_weights()
            .map(|node| node.payload().clone())
            .collect::<Vec<_>>();

        let add_enode = payloads
            .iter()
            .flat_map(|payload| payload.enodes.values().flatten())
            .find(|enode| enode.func_offset.func == "Add")
            .expect("Add enode should be present");

        assert_eq!(add_enode.display_label.as_deref(), Some("2 + 3"));
        let metadata = add_enode
            .dsl_metadata
            .as_ref()
            .expect("Add enode should carry DSL metadata");
        assert_eq!(metadata.typst_template.as_deref(), Some("{lhs} + {rhs}"));
        assert_eq!(metadata.precedence, 10);
    }
}
