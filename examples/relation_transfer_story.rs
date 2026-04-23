use eggplant::egglog;
use eggplant::{prelude::*, tx_rx_vt_pr, wrap::EgglogEnumVariantTy};

const ALICE_ID: i64 = 1;
const BOB_ID: i64 = 2;
const CAROL_ID: i64 = 3;
const RING_ID: i64 = 1001;
const BOOK_ID: i64 = 2002;

#[eggplant::dsl]
enum Person {
    Human { id: i64 },
}

impl<T: eggplant::wrap::NodeDropperSgl, V: EgglogEnumVariantTy> std::fmt::Debug for Person<T, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.cur_sym())
    }
}

#[eggplant::relation]
struct Owns {
    owner: Person,
    item_id: i64,
}

#[eggplant::relation]
struct TransferRequest {
    new_owner: Person,
    item_id: i64,
}

#[eggplant::func(output = bool, no_merge)]
struct OwnershipSeen {
    owner_id: i64,
    item_id: i64,
}

#[eggplant::func(output = bool, no_merge)]
struct TransferApplied {
    item_id: i64,
    from_owner_id: i64,
    to_owner_id: i64,
}

tx_rx_vt_pr!(OwnershipTx, OwnershipPatRec);

fn main() {
    let _ = env_logger::try_init();

    let ruleset = OwnershipTx::new_ruleset("relation_transfer_story");
    OwnershipTx::add_rule(
        "seed_story",
        ruleset,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _| {
            let alice = ctx.insert_human(ALICE_ID);
            let bob = ctx.insert_human(BOB_ID);
            let carol = ctx.insert_human(CAROL_ID);
            ctx.insert_owns(alice, RING_ID);
            ctx.insert_owns(carol, BOOK_ID);
            ctx.insert_transfer_request(bob, RING_ID);
        },
    );
    OwnershipTx::add_rule(
        "mark_ownership",
        ruleset,
        || {
            let owner = Human::query();
            let owns = Owns::query(&owner);
            #[eggplant::pat_vars]
            struct Pat {
                owns: Owns,
                owner: Human,
            }
            Pat::new(owns, owner)
        },
        |ctx, pat| {
            let owner_id = ctx.devalue(pat.owner.id);
            let item_id = ctx.devalue(pat.owns.item_id);
            ctx.set_ownership_seen(owner_id, item_id, true);
        },
    );
    OwnershipTx::add_rule(
        "apply_transfer",
        ruleset,
        || {
            let current_owner = Human::query();
            let owns = Owns::query(&current_owner);
            let new_owner = Human::query();
            let request = TransferRequest::query(&new_owner);
            let same_item = owns.handle_item_id().eq(&request.handle_item_id());
            let owner_changes = current_owner.handle().ne(&new_owner.handle());
            #[eggplant::pat_vars]
            struct Pat {
                owns: Owns,
                current_owner: Human,
                request: TransferRequest,
                new_owner: Human,
            }
            Pat::new(owns, current_owner, request, new_owner)
                .assert(same_item)
                .assert(owner_changes)
        },
        |ctx, pat| {
            let item_id = ctx.devalue(pat.owns.item_id);
            let from_owner_id = ctx.devalue(pat.current_owner.id);
            let to_owner_id = ctx.devalue(pat.new_owner.id);
            // Relation actions are insert-only today, so transfer is modeled as
            // deriving the recipient's ownership fact plus a separate marker.
            ctx.insert_owns(pat.new_owner, item_id);
            ctx.set_transfer_applied(item_id, from_owner_id, to_owner_id, true);
        },
    );

    let report = OwnershipTx::run_ruleset(ruleset, RunConfig::Sat);
    println!("matches: {:?}", report.num_matches_per_rule);
    let mark_matches = report
        .num_matches_per_rule
        .get("@mark_ownership")
        .copied()
        .unwrap_or(0);
    let transfer_matches = report
        .num_matches_per_rule
        .get("@apply_transfer")
        .copied()
        .unwrap_or(0);

    assert!(
        mark_matches >= 3,
        "expected at least the seeded ownership facts to be observed"
    );
    assert_eq!(
        transfer_matches, 1,
        "expected exactly one transfer application in the story"
    );
    println!("ownership observations recorded: {mark_matches}");
    println!("transfer applications recorded: {transfer_matches}");
}
