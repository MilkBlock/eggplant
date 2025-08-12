use std::{fs::File, io::Write, path::PathBuf};

use egglog::{Term, TermDag};
use petgraph::{
    EdgeType,
    dot::{Config, Dot},
    prelude::StableGraph,
};

pub fn topo_sort(term_dag: &TermDag) -> Vec<usize> {
    // init in degrees and out degrees
    let mut parents = Vec::new();
    let mut outs = Vec::new();
    parents.resize(term_dag.size(), Vec::new());
    outs.resize(term_dag.size(), 0);
    for (i, out_degree) in outs.iter_mut().enumerate() {
        let term = term_dag.get(i);
        *out_degree = match term {
            Term::Lit(_) => usize::MAX,
            Term::Var(_) => panic!(),
            Term::App(_, items) => items.iter().map(|x| parents[*x].push(i)).count(),
        }
    }
    let mut rst = Vec::new();
    let mut wait_for_release = Vec::new();
    // start node should not have any out edges in subgraph
    for (idx, _value) in outs.iter().enumerate() {
        if usize::MAX == outs[idx] || 0 == outs[idx] {
            wait_for_release.push(idx);
        }
    }
    log::debug!("wait for release {:?}", wait_for_release);
    log::debug!("parents {:?}", parents);
    log::debug!("outs {:?}", outs);
    while !wait_for_release.is_empty() {
        let popped = wait_for_release.pop().unwrap();
        for &parent in &parents[popped] {
            outs[parent] -= 1;
            if outs[parent] == 0 {
                log::debug!(" {} found to be 0", parent);
                wait_for_release.push(parent);
            }
        }
        if outs[popped] != usize::MAX {
            rst.push(popped);
        }
    }
    log::debug!("topo sort:{:?}", rst);
    rst
}

pub fn generate_dot_by_graph<N: std::fmt::Debug, E: std::fmt::Debug, Ty: EdgeType>(
    g: &StableGraph<N, E, Ty>,
    path: PathBuf,
    graph_config: &[Config],
) {
    let dot_name = path.clone();
    let mut f = File::create(dot_name.clone()).unwrap();
    let dot_string = format!("{:?}", Dot::with_config(&g, &graph_config));
    f.write_all(dot_string.as_bytes()).expect("写入失败");
}
