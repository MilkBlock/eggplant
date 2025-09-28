# eggplant

`eggplant` is the High-Level Rust API repo for the `egglog` tool accompanying the paper
  "Better Together: Unifying Datalog and Equality Saturation"
  ([ACM DL](https://dl.acm.org/doi/10.1145/3591239), [arXiv](https://arxiv.org/abs/2304.04332)).

It is hard to do Graph operations directly on EGraph because EGraph is a highly compressed data structure.

Based on that fact, `eggplant` provides out-of-box Graph API that allows you to do revisions on EGraph intuitively.

There is also a Proc-Macro library for users to quickly define a suite of DSL.

## Motivation

Recently, I've been researching the implementation of egglog. Since the egglog ecosystem is not yet mature, I spent some time providing a better API implementation with procedural macros and compile-time type checking for rust-native. This includes:

1. **DSL Define API**: Quickly define a set of DSL
2. **Pattern Define API**: Pattern definition for pattern matching, using declarative definition approach
3. **Commit API**: Since egraph is a highly compressed structure that compresses node relationship information and is naturally averse to deletion operations, we need a bridge between normal graph structures and egraph. I borrowed from git's commit API for batch commits and version control.
4. **Insert API**: Insert API with compile-time type checking added on top of the original, so you no longer need to nervously add nodes like with the native API
5. **Run Rule API**: Run Pattern Rewrite

Finally, this forms a framework that allows users to implement a pattern rewriting framework supporting integer addition, subtraction, multiplication, and division with constant propagation in just fifty lines of code.

## DSL Define API

```rust
#[eggplant::dsl]
pub enum Expr {
    Const { num: i64 },
    Mul { l: Expr, r: Expr },
    Sub { l: Expr, r: Expr },
    Add { l: Expr, r: Expr },
    Div { l: Expr, r: Expr },
}
```

Yes, you read that correctly. Our addition, subtraction, multiplication, and division constant DSL is defined just like that. Using Rust's Sum Type can represent a set of DSL very well, which is very intuitive.

## Pattern Define API

Here's an example of addition pattern definition API:

```rust
#[eggplant::pat_vars]
struct AddPat {
    l: Const,
    r: Const,
    p: Add,
}

let pat = || {
    // mention here we use query() since it's a variant type, if we want to query an expression
    // we should use query_leaf()
    let l = Const::query(); 
    let r = Const::query();
    let p = Add::query(&l, &r);
    AddPat::new(l, r, p)
}
```

Note that currently we must add a generic parameter after `AddPat`. I think we can use procedural macro tricks to omit this generic parameter later :) (Now supports omitting generics).

Defining a Pattern is actually defining two things: what to extract from the Pattern for computation and what this Pattern looks like. From the user's perspective, using a declarative approach to define is definitely more convenient. Here we use a closure to define a Pattern, but as the generic parameter exposed by the struct above shows, there's a global singleton managing all Patterns and submitting them to the database at the appropriate time.

## Addition and Pattern Rewrite Definition!

```rust
let action = |ctx, values| {
    let cal = ctx.devalue(values.l.num) + ctx.devalue(values.r.num); // Used as struct rather than enum
    let add_value = ctx.insert_const(cal);
    ctx.union(values.p, add_value);
},
```

You can see that we still need to use `ctx.devalue` to convert from database ID to real data. Through compile-time type information deduction, we can save the type annotation for `devalue`. Also, the procedural macro generates APIs corresponding to the DSL for `ctx`, maximizing the power of intellisense and compile-time type information.

## Running

```rust
tx_rx_vt_pr!(MyTx, MyPatRec);  // Global singleton definition for storing graph and patterns
fn main() {
    let expr: Expr<MyTx, AddTy> =
        Add::new(&Mul::new(&Const::new(3), &Const::new(2)), &Const::new(4));
    expr.commit();

    let ruleset = MyTx::new_ruleset("constant_prop");
    prop!(Add,+,AddPat,ruleset);
    prop!(Sub,-,SubPat,ruleset);
    prop!(Mul,*,MulPat,ruleset);
    prop!(Div,/,DivPat,ruleset);
    for _ in 0..4 {
        let _ = MyTx::run_ruleset(ruleset, RunConfig::None);
    }
    MyTx::sgl().egraph_to_dot("egraph.dot".into());
}
```

Finally, the following EGraph is generated, and you can see that the root node value is directly derived.

Note that the execution count of `run_ruleset` is not the number of matches, but should be less than the tree depth.

## Dependencies

Using eggplant requires three dependencies:

```toml
eggplant = { git = "https://github.com/MilkBlock/eggplant" }
derive_more = "2.0.1"
strum = "0.27.2"
```

Since egglog's database backend is not yet released on crate.io, I'm using git links to add dependencies here. This will be updated as egglog becomes stable and released.

## Future Work

Of course, this framework still lacks some other APIs such as:

- Adding Predicate API to pattern definitions as constraint conditions for pattern judgment (currently can be solved by judging in action)
- Supporting pattern references for writing complex patterns (feels relatively easy to implement)
- Cost attributes for extracting optimal values

## Complete Code

Here's the complete code for implementing addition, subtraction, multiplication, and division constant propagation:

```rust
use eggplant::{prelude::*, tx_rx_vt_pr};

#[eggplant::ty]
pub enum Expr {
    Const { num: i64 },
    Mul { l: Expr, r: Expr },
    Sub { l: Expr, r: Expr },
    Add { l: Expr, r: Expr },
    Div { l: Expr, r: Expr },
}

tx_rx_vt_pr!(MyTx, MyPatRec);

macro_rules! prop {
    ($ty:ident,$op:tt,$pat_name:ident,$ruleset:ident) => {
        #[eggplant::pat_vars]
        struct $pat_name {
            l: Const,
            r: Const,
            p: $ty,
        }
        MyTx::add_rule(
            stringify!($pat_name),
            $ruleset,
            || {
                let l = Const::query();
                let r = Const::query();
                let p = $ty::query(&l, &r);
                $pat_name::new(l, r, p)
            },
            |ctx, values| {
                let cal = ctx.devalue(values.l.num) $op ctx.devalue(values.r.num);
                let op_value = ctx.insert_const(cal);
                ctx.union(values.p, op_value);
            },
        );
    };
}

fn main() {
    let expr: Expr<MyTx, AddTy> =
        Add::new(&Mul::new(&Const::new(3), &Const::new(2)), &Const::new(4));
    expr.commit();

    let ruleset = MyTx::new_ruleset("constant_prop");
    prop!(Add,+,AddPat,ruleset);
    prop!(Sub,-,SubPat,ruleset);
    prop!(Mul,*,MulPat,ruleset);
    prop!(Div,/,DivPat,ruleset);
    for _ in 0..4 {
        let _ = MyTx::run_ruleset(ruleset, RunConfig::None);
    }
    MyTx::sgl().egraph_to_dot("egraph.dot".into());
}
```

## Documentation

To view documentation, run `cargo doc --open`.

## Contributing

Welcome to submit issues! Hope you have fun!

GitHub repository: [https://github.com/MilkBlock/eggplant](https://github.com/MilkBlock/eggplant)


