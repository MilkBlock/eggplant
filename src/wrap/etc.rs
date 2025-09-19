use std::{fmt, fs::File, hash::Hash, io::Write, marker::PhantomData, path::PathBuf};

use crate::wrap::{self, EgglogTy, VecContainer};
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
pub(crate) fn quote(s: &str) -> String {
    format!("{:?}", s)
}

pub(crate) struct Escape<'a>(pub &'a str);

impl<'a> fmt::Display for Escape<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Because the internet is always right, turns out there's not that many
        // characters to escape: http://stackoverflow.com/questions/7381974
        let Escape(s) = *self;
        let pile_o_bits = s;
        let mut last = 0;
        for (i, ch) in s.char_indices() {
            let s = match ch {
                '>' => "&gt;",
                '<' => "&lt;",
                '&' => "&amp;",
                '\'' => "&#39;",
                '"' => "&quot;",
                '\n' => "<br/>",
                _ => continue,
            };
            fmt.write_str(&pile_o_bits[last..i])?;
            fmt.write_str(s)?;
            // NOTE: we only expect single byte characters here - which is fine as long as we
            // only match single byte characters
            last = i + 1;
        }

        if last < s.len() {
            fmt.write_str(&pile_o_bits[last..])?;
        }
        Ok(())
    }
}

impl<T: EgglogTy> Eq for VecContainer<T> {}
impl<T: EgglogTy> PartialEq for VecContainer<T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}
impl<T: EgglogTy> Hash for VecContainer<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}

unsafe impl<T: EgglogTy> Send for VecContainer<T> {}
unsafe impl<T: EgglogTy> Sync for VecContainer<T> {}
impl<T: EgglogTy> Clone for VecContainer<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _p: self._p.clone(),
        }
    }
}

impl<T: EgglogTy> VecContainer<T> {
    pub fn new() -> VecContainer<T> {
        VecContainer {
            inner: egglog::sort::VecContainer {
                do_rebuild: false,
                data: vec![],
            },
            _p: PhantomData,
        }
    }
    pub fn from_raw_vec(v: Vec<egglog::Value>) -> VecContainer<T> {
        VecContainer {
            inner: egglog::sort::VecContainer {
                do_rebuild: false,
                data: v,
            },
            _p: PhantomData,
        }
    }
    pub fn iter(&self) -> impl Iterator<Item = wrap::Value<T>> {
        self.inner
            .data
            .as_slice()
            .iter()
            .map(|x| wrap::Value::new(*x))
            .into_iter()
    }
}
