use eggplant::{basic_tx_vt, prelude::*};
use std::path::PathBuf;

// Type aliases for Vec types
#[eggplant::ty]
struct VecCtl {
    v: Vec<Ctl>,
}

#[eggplant::ty]
struct VecWF {
    v: Vec<WeightedFn>,
}

#[eggplant::ty]
struct VecHitBox {
    v: Vec<HitBox>,
}

#[eggplant::ty]
struct Points {
    v: Vec<Point>,
}

#[eggplant::ty]
enum Ctl {
    Para { vec_ctl: VecCtl },
    Seq { vec_ctl: VecCtl },
    Await { ctl: Ctl },
    Atom { anim_atom: AnimAtom },
}
#[eggplant::ty]
enum AnimAtom {
    Anim {
        object: BRabjectInstance,
        path: Path,
        duration: Duration,
        rate_cfg: RateCfg,
    },
    ConstructAnim {
        from: BRabjectInstance,
        to: BRabject,
        path: Path,
        duration: Duration,
        rate_cfg: RateCfg,
    },
    DestructAnim {
        from: BRabjectInstance,
        to: BRabject,
        path: Path,
        duration: Duration,
        rate_cfg: RateCfg,
    },
}
#[eggplant::ty]
enum BRabjectInstance {
    Instance { template: BRabject },
}

#[eggplant::ty]
enum BRabject {
    ColoredShape { shape: Shape, color: Color },
    Text { position: Point, content: String },
}

#[eggplant::ty]
enum Color {
    Srgba {
        red: f64,
        green: f64,
        blue: f64,
        alpha: f64,
    },
}

#[eggplant::ty]
enum Shape {
    Polygon { points: Points },
}

#[eggplant::ty]
enum Duration {
    DurationBySecs { seconds: f64 },
    DurationByMili { milliseconds: f64 },
}

#[eggplant::ty]
enum BezierPathBuilder {
    Quadratic {
        control: Point,
        end: Point,
        rest: BezierPathBuilder,
    },
    Cubic {
        control1: Point,
        control2: Point,
        end: Point,
        rest: BezierPathBuilder,
    },
    LineTo {
        to: Point,
        rest: BezierPathBuilder,
    },
    Start {
        at: Point,
        rest: BezierPathBuilder,
    },
    PathEnd {},
}

#[eggplant::ty]
enum Offset {
    DVec3 { x: f64, y: f64, z: f64 },
    DVec2 { x: f64, y: f64 },
}

#[eggplant::ty]
enum Point {
    FixedPoint { offset: Offset },
    OffsetPoint { offset: Offset, base: Point },
    CurAnchorOf { object: BRabject },
    PointAtIdx { shape: Shape, index: i64 },
}

#[eggplant::ty]
enum Weight {
    W { value: f64 },
}

#[eggplant::ty]
enum BuiltinF {
    Lerp {},
    Stay {},
}

#[eggplant::ty]
enum Fn {
    Builtin { function: BuiltinF },
    WasmGuestExtern { name: String },
}

#[eggplant::ty]
enum WeightedFn {
    WF { f: Fn, w: Weight }, // 作为元组字段
}

#[eggplant::ty]
enum RateCfg {
    RateFn { wfs: VecWF },
}

#[eggplant::ty]
enum Path {
    BezierPath {
        bezier_path_builder: BezierPathBuilder,
    },
}

#[eggplant::ty]
enum HitBox {
    ShapedBox { shape: Shape },
    HitBoxs { histboxs: VecHitBox },
}

#[eggplant::func(output = Ctl)]
struct CurrentTimeline {}

fn main() {
    env_logger::init();
    // three points
    let p1 = FixedPoint::new(&DVec2::new(1.0, 1.0));
    let p2 = FixedPoint::new(&DVec2::new(1.0, 2.0));
    let p3 = OffsetPoint::new(&DVec2::new(1.0, 2.0), &p2);

    // point vec
    let points = Points::new(vec![&p1, &p2, &p3]);

    // triangle
    let triangle_shape = Shape::new_polygon(&points);

    // red triangle
    let triangle = ColoredShape::new(&triangle_shape, &Srgba::new(1.0, 0.0, 0.0, 1.0));
    let triangle_instance = Instance::new(&triangle);

    // anchor
    let cur_anchor = CurAnchorOf::new(&triangle);

    // target basing on offset from cur_anchor
    let target_point = OffsetPoint::new(&DVec2::new(1.0, 1.0), &cur_anchor);

    // path
    let path_end = PathEnd::new();
    let line_to = LineTo::new(&target_point, &path_end);
    let start = Start::new(&cur_anchor, &line_to);
    let path = BezierPath::new(&start);

    // anim atom
    let anim_atom = Anim::new(
        &triangle_instance,
        &path,
        &DurationBySecs::new(3.0),
        &RateFn::new(&VecWF::new(vec![&WeightedFn::new_wf(
            &Builtin::new(&Lerp::new()),
            &W::new(1.0),
        )])),
    );

    // 构建动画序列
    let atom = Atom::new(&anim_atom);
    let seq = Seq::new(&VecCtl::new(vec![&atom]));

    // 构建并行时间线
    let s = VecCtl::new(vec![&seq]);
    let s2 = VecCtl::new(vec![&seq, &&seq]);
    let mut timeline = Para::<MyTx>::new(&s);
    timeline.commit();
    timeline.set_vec_ctl(&s2);

    CurrentTimeline::set((), &timeline);
    // 输出到dot文件
    MyTx::sgl().to_dot(PathBuf::from("timeline_egraph"));
}

basic_tx_vt!(MyTx);
