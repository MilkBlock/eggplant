use eggplant::basic_tx_vt;
use eggplant::{eggplant_func, eggplant_ty};
use std::path::PathBuf;

// Type aliases for Vec types
#[eggplant_ty]
struct VecCtl {
    v: Vec<Ctl>,
}

#[eggplant_ty]
struct VecWF {
    v: Vec<WeightedFn>,
}

#[eggplant_ty]
struct VecHitBox {
    v: Vec<HitBox>,
}

#[eggplant_ty]
struct Points {
    v: Vec<Point>,
}

#[eggplant_ty]
enum Ctl {
    Para { vec_ctl: VecCtl },
    Seq { vec_ctl: VecCtl },
    Await { ctl: Box<Ctl> },
    Atom { anim_atom: AnimAtom },
}
#[eggplant_ty]
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
#[eggplant_ty]
enum BRabjectInstance {
    Instance { template: BRabject },
}

#[eggplant_ty]
enum BRabject {
    ColoredShape { shape: Shape, color: Color },
    Text { position: Point, content: String },
}

#[eggplant_ty]
enum Color {
    Srgba {
        red: f64,
        green: f64,
        blue: f64,
        alpha: f64,
    },
}

#[eggplant_ty]
enum Shape {
    Polygon { points: Points },
}

#[eggplant_ty]
enum Duration {
    DurationBySecs { seconds: f64 },
    DurationByMili { milliseconds: f64 },
}

#[eggplant_ty]
enum BezierPathBuilder {
    Quadratic {
        control: Point,
        end: Point,
        rest: Box<BezierPathBuilder>,
    },
    Cubic {
        control1: Point,
        control2: Point,
        end: Point,
        rest: Box<BezierPathBuilder>,
    },
    LineTo {
        to: Point,
        rest: Box<BezierPathBuilder>,
    },
    Start {
        at: Point,
        rest: Box<BezierPathBuilder>,
    },
    PathEnd {},
}

#[eggplant_ty]
enum Offset {
    DVec3 { x: f64, y: f64, z: f64 },
    DVec2 { x: f64, y: f64 },
}

#[eggplant_ty]
enum Point {
    FixedPoint { offset: Offset },
    OffsetPoint { offset: Offset, base: Box<Point> },
    CurAnchorOf { object: Box<BRabject> },
    PointAtIdx { shape: Shape, index: i64 },
}

#[eggplant_ty]
enum Weight {
    W { value: f64 },
}

#[eggplant_ty]
enum BuiltinF {
    Lerp {},
    Stay {},
}

#[eggplant_ty]
enum Fn {
    Builtin { function: BuiltinF },
    WasmGuestExtern { name: String },
}

#[eggplant_ty]
enum WeightedFn {
    WF { f: Fn, w: Weight }, // 作为元组字段
}

#[eggplant_ty]
enum RateCfg {
    RateFn { wfs: VecWF },
}

#[eggplant_ty]
enum Path {
    BezierPath {
        bezier_path_builder: BezierPathBuilder,
    },
}

#[eggplant_ty]
enum HitBox {
    ShapedBox { shape: Shape },
    HitBoxs { histboxs: VecHitBox },
}

#[eggplant_func(output = Ctl)]
struct CurrentTimeline {}

fn main() {
    env_logger::init();
    // three points
    let p1 = Point::<MyRx>::new_fixed_point(&Offset::new_d_vec2(1.0, 1.0));
    let p2 = Point::new_fixed_point(&Offset::new_d_vec2(1.0, 2.0));
    let p3 = Point::new_offset_point(&Offset::new_d_vec2(1.0, 2.0), &p2);

    // point vec
    let points = Points::new(vec![&p1, &p2, &p3]);

    // triangle
    let triangle_shape = Shape::new_polygon(&points);

    // red triangle
    let triangle =
        BRabject::new_colored_shape(&triangle_shape, &Color::new_srgba(1.0, 0.0, 0.0, 1.0));
    let triangle_instance = BRabjectInstance::new_instance(&triangle);

    // anchor
    let cur_anchor = Point::new_cur_anchor_of(&triangle);

    // target basing on offset from cur_anchor
    let target_point = Point::new_offset_point(&Offset::new_d_vec2(1.0, 1.0), &cur_anchor);

    // path
    let path_end = BezierPathBuilder::new_path_end();
    let line_to = BezierPathBuilder::new_line_to(&target_point, &path_end);
    let start = BezierPathBuilder::new_start(&cur_anchor, &line_to);
    let path = Path::new_bezier_path(&start);

    // anim atom
    let anim_atom = AnimAtom::new_anim(
        &triangle_instance,
        &path,
        &Duration::new_duration_by_secs(3.0),
        &RateCfg::new_rate_fn(&VecWF::new(vec![&WeightedFn::new_wf(
            &Fn::new_builtin(&BuiltinF::new_lerp()),
            &Weight::new_w(1.0),
        )])),
    );

    // 构建动画序列
    let atom = Ctl::new_atom(&anim_atom);
    let seq = Ctl::new_seq(&VecCtl::new(vec![&atom]));

    // 构建并行时间线
    let s = VecCtl::new(vec![&seq]);
    let s2 = VecCtl::new(vec![&seq, &&seq]);
    let mut timeline = Ctl::new_para(&s);
    timeline.commit();
    timeline.set_vec_ctl(&s2);

    CurrentTimeline::set((), &timeline);
    // 输出到dot文件
    MyRx::sgl().to_dot(PathBuf::from("timeline_egraph"));
}

basic_tx_vt!(MyRx);
