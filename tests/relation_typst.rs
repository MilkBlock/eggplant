use eggplant::{self, inventory};
use eggplant::prelude::*;
use eggplant::wrap::Decl;

#[eggplant::relation(typst = "edge({src}, {dst})", precedence = 17)]
struct TypstEdge {
    src: i64,
    dst: i64,
}

#[allow(non_camel_case_types)]
#[eggplant::func(output = bool, no_merge, typst = "touch({x})", precedence = 70)]
struct persisted_meta_touch {
    x: i64,
}

#[test]
fn relation_typst_metadata_is_registered() {
    let registered = inventory::iter::<Decl>
        .into_iter()
        .find_map(|decl| match decl {
            Decl::EgglogRelationTy {
                name,
                input,
                typst_template,
                precedence,
            } if *name == "TypstEdge" => Some((*input, *typst_template, *precedence)),
            _ => None,
        })
        .expect("TypstEdge relation declaration should be registered");

    assert_eq!(registered.0, ["i64", "i64"]);
    assert_eq!(registered.1, Some("edge({src}, {dst})"));
    assert_eq!(registered.2, 17);
}

#[test]
fn func_typst_metadata_is_registered() {
    let registered = inventory::iter::<Decl>
        .into_iter()
        .find_map(|decl| match decl {
            Decl::EgglogFuncTy {
                name,
                typst_template,
                merge,
                precedence,
                ..
            } if *name == "persisted_meta_touch" => Some((*typst_template, *merge, *precedence)),
            _ => None,
        })
        .expect("persisted_meta_touch function declaration should be registered");

    assert_eq!(registered.0, Some("touch({x})"));
    assert_eq!(registered.1, None);
    assert_eq!(registered.2, 70);
}
