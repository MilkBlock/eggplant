pub mod pat_rec;
pub mod session_runtime;
pub mod tx;
pub mod tx_minimal;
pub mod tx_rx_vt;
pub mod tx_rx_vt_pr;
pub mod tx_rx_vt_pr_slot;
pub mod tx_vt;

/// macro to quickly define a Transimitter with no version control
#[macro_export]
macro_rules! basic_tx_no_vt {
    ($name:ident) => {
        pub struct $name {
            tx: eggplant::instances::tx::TxNoVT,
        }
        impl eggplant::prelude::SingletonGetter for $name {
            type RetTy = eggplant::instances::tx::TxNoVT;
            fn sgl() -> &'static eggplant::instances::tx::TxNoVT {
                static INSTANCE: std::sync::OnceLock<$name> = std::sync::OnceLock::new();
                &INSTANCE
                    .get_or_init(|| -> $name {
                        Self {
                            tx: eggplant::instances::tx::TxNoVT::new(),
                        }
                    })
                    .tx
            }
        }
        impl eggplant::wrap::NonPatRecSgl for $name {
            fn egraph() -> std::sync::Arc<std::sync::Mutex<egglog::EGraph>> {
                Self::sgl().egraph.clone()
            }
        }
    };
}
/// macro to quickly define a Transimitter with version control
#[macro_export]
macro_rules! basic_tx_vt {
    ($name:ident) => {
        pub struct $name {
            tx: eggplant::instances::tx_vt::TxVT,
        }
        impl eggplant::prelude::SingletonGetter for $name {
            type RetTy = eggplant::instances::tx_vt::TxVT;
            fn sgl() -> &'static eggplant::instances::tx_vt::TxVT {
                static INSTANCE: std::sync::OnceLock<$name> = std::sync::OnceLock::new();
                &INSTANCE
                    .get_or_init(|| -> $name {
                        Self {
                            tx: eggplant::instances::tx_vt::TxVT::new(),
                        }
                    })
                    .tx
            }
        }
        impl eggplant::wrap::NonPatRecSgl for $name {
            fn egraph() -> std::sync::Arc<std::sync::Mutex<egglog::EGraph>> {
                Self::sgl().egraph.clone()
            }
        }
    };
}
/// macro to quickly define a minimal Transimitter
#[macro_export]
macro_rules! basic_tx_minimal {
    ($name:ident) => {
        pub struct $name {
            tx: eggplant::instances::tx_minimal::TxMinimal,
        }
        impl eggplant::prelude::SingletonGetter for $name {
            type RetTy = eggplant::instances::tx_minimal::TxMinimal;
            fn sgl() -> &'static eggplant::instances::tx_minimal::TxMinimal {
                static INSTANCE: std::sync::OnceLock<$name> = std::sync::OnceLock::new();
                &INSTANCE
                    .get_or_init(|| -> $name {
                        Self {
                            tx: eggplant::instances::tx_minimal::TxMinimal::new(),
                        }
                    })
                    .tx
            }
        }
        impl eggplant::wrap::NonPatRecSgl for $name {
            fn egraph() -> std::sync::Arc<std::sync::Mutex<egglog::EGraph>> {
                Self::sgl().egraph.clone()
            }
        }
    };
}

#[macro_export]
macro_rules! basic_tx_rx_vt {
    ($name:ident) => {
        pub struct $name {
            tx: eggplant::instances::tx_rx_vt::TxRxVT,
        }
        impl eggplant::prelude::SingletonGetter for $name {
            type RetTy = eggplant::instances::tx_rx_vt::TxRxVT;
            fn sgl() -> &'static eggplant::instances::tx_rx_vt::TxRxVT {
                static INSTANCE: std::sync::OnceLock<$name> = std::sync::OnceLock::new();
                &INSTANCE
                    .get_or_init(|| -> $name {
                        Self {
                            tx: eggplant::instances::tx_rx_vt::TxRxVT::new(),
                        }
                    })
                    .tx
            }
        }
        impl eggplant::wrap::NonPatRecSgl for $name {
            fn egraph() -> std::sync::Arc<std::sync::Mutex<egglog::EGraph>> {
                Self::sgl().egraph.clone()
            }
        }
    };
}

#[macro_export]
macro_rules! basic_tx_rx_vt_pr {
    ($name:ident) => {
        pub struct $name {
            tx: eggplant::instances::tx_rx_vt_pr::TxRxVTPR,
        }
        impl eggplant::prelude::SingletonGetter for $name {
            type RetTy = eggplant::instances::tx_rx_vt_pr::TxRxVTPR;
            fn sgl() -> &'static eggplant::instances::tx_rx_vt_pr::TxRxVTPR {
                static INSTANCE: std::sync::OnceLock<$name> = std::sync::OnceLock::new();
                &INSTANCE
                    .get_or_init(|| -> $name {
                        Self {
                            tx: eggplant::instances::tx_rx_vt_pr::TxRxVTPR::new(),
                        }
                    })
                    .tx
            }
        }
        impl eggplant::wrap::NonPatRecSgl for $name {
            fn egraph() -> std::sync::Arc<std::sync::Mutex<egglog::EGraph>> {
                Self::sgl().egraph.clone()
            }
        }
    };
}

#[macro_export]
macro_rules! tx_rx_vt_pr {
    ($tx_name:ident, $pat_rec_name:ident) => {
        pub struct $tx_name;
        pub struct $pat_rec_name;

        impl $tx_name {
            pub fn new_session() -> eggplant::instances::session_runtime::Session<Self> {
                <Self as eggplant::instances::session_runtime::SessionTxSgl>::new_session()
            }

            pub fn default_session() -> eggplant::instances::session_runtime::Session<Self> {
                <Self as eggplant::instances::session_runtime::SessionTxSgl>::default_session()
            }

            pub fn get_or_register_ruleset(
                key: &'static str,
                build: impl FnOnce() -> eggplant::prelude::RuleSetId,
            ) -> eggplant::prelude::RuleSetId {
                eggplant::instances::session_runtime::get_or_register_ruleset::<Self>(key, build)
            }

            pub fn egraph() -> std::sync::Arc<std::sync::Mutex<egglog::EGraph>> {
                eggplant::instances::session_runtime::egraph::<Self>()
            }

            pub fn reset_for_bench() {
                eggplant::instances::session_runtime::reset_for_bench::<Self>()
            }
        }

        impl eggplant::instances::session_runtime::SessionPatRecMarker for $pat_rec_name {
            type TxMarker = $tx_name;
            type MetaTy =
                <eggplant::instances::pat_rec::PatRecorder as eggplant::wrap::PatRec>::MetaTy;
        }

        impl eggplant::instances::session_runtime::SessionTxMarker for $tx_name {
            type Runtime = eggplant::instances::tx_rx_vt_pr::TxRxVTPR;
            type PatRecorder = eggplant::instances::pat_rec::PatRecorder;
            type PatRecMarker = $pat_rec_name;

            fn new_runtime() -> Self::Runtime {
                eggplant::instances::tx_rx_vt_pr::TxRxVTPR::new()
            }

            fn new_pat_recorder() -> Self::PatRecorder {
                eggplant::instances::pat_rec::PatRecorder::new()
            }

            fn runtime_egraph(
                runtime: &Self::Runtime,
            ) -> std::sync::Arc<std::sync::Mutex<egglog::EGraph>> {
                runtime.egraph.clone()
            }

            fn reset_runtime_for_bench(runtime: &Self::Runtime) {
                runtime.reset_for_bench()
            }
        }

        impl eggplant::prelude::SingletonGetter for $tx_name {
            type RetTy = eggplant::instances::session_runtime::TxFacade<$tx_name>;

            fn sgl() -> &'static Self::RetTy {
                static FACADE: eggplant::instances::session_runtime::TxFacade<$tx_name> =
                    eggplant::instances::session_runtime::TxFacade::new();
                &FACADE
            }
        }

        impl eggplant::prelude::SingletonGetter for $pat_rec_name {
            type RetTy = eggplant::instances::session_runtime::PatRecFacade<$pat_rec_name>;

            fn sgl() -> &'static Self::RetTy {
                static FACADE: eggplant::instances::session_runtime::PatRecFacade<$pat_rec_name> =
                    eggplant::instances::session_runtime::PatRecFacade::new();
                &FACADE
            }
        }

        impl eggplant::wrap::NonPatRecSgl for $tx_name {
            fn egraph() -> std::sync::Arc<std::sync::Mutex<egglog::EGraph>> {
                eggplant::instances::session_runtime::egraph::<$tx_name>()
            }
        }

        impl eggplant::wrap::WithPatRecSgl for $tx_name {
            type PatRecSgl = $pat_rec_name;
        }
        impl eggplant::wrap::WithRxSgl for $pat_rec_name {
            type RxSgl = $tx_name;
        }
    };
}

#[macro_export]
macro_rules! basic_slotted_tx_rx_vt_pr {
    ($name:ident) => {
        pub struct $name {
            tx: eggplant::instances::tx_rx_vt_pr_slot::SlottedTxRxVTPR,
        }
        impl eggplant::prelude::SingletonGetter for $name {
            type RetTy = eggplant::instances::tx_rx_vt_pr_slot::SlottedTxRxVTPR;
            fn sgl() -> &'static eggplant::instances::tx_rx_vt_pr_slot::SlottedTxRxVTPR {
                static INSTANCE: std::sync::OnceLock<$name> = std::sync::OnceLock::new();
                &INSTANCE
                    .get_or_init(|| -> $name {
                        Self {
                            tx: eggplant::instances::tx_rx_vt_pr_slot::SlottedTxRxVTPR::new(),
                        }
                    })
                    .tx
            }
        }
        impl eggplant::wrap::NonPatRecSgl for $name {
            fn egraph() -> std::sync::Arc<std::sync::Mutex<egglog::EGraph>> {
                Self::sgl().egraph.clone()
            }
        }
    };
}

#[macro_export]
macro_rules! slotted_tx_rx_vt_pr {
    ($tx_name:ident, $pat_rec_name:ident) => {
        eggplant::basic_slotted_tx_rx_vt_pr!($tx_name);
        eggplant::slotted_patttern_recorder!($pat_rec_name);
        impl eggplant::wrap::WithPatRecSgl for $tx_name {
            type PatRecSgl = $pat_rec_name;
        }
        impl eggplant::wrap::WithRxSgl for $pat_rec_name {
            type RxSgl = $tx_name;
        }
    };
}

#[macro_export]
macro_rules! basic_patttern_recorder {
    ($name:ident) => {
        #[derive(Debug)]
        pub struct $name {
            tx: eggplant::instances::pat_rec::PatRecorder,
        }
        impl eggplant::prelude::SingletonGetter for $name {
            type RetTy = eggplant::instances::pat_rec::PatRecorder;
            fn sgl() -> &'static eggplant::instances::pat_rec::PatRecorder {
                static INSTANCE: std::sync::OnceLock<$name> = std::sync::OnceLock::new();
                &INSTANCE
                    .get_or_init(|| -> $name {
                        Self {
                            tx: eggplant::instances::pat_rec::PatRecorder::new(),
                        }
                    })
                    .tx
            }
        }
    };
}

#[macro_export]
macro_rules! slotted_patttern_recorder {
    ($name:ident) => {
        #[derive(Debug)]
        pub struct $name {
            tx: eggplant::instances::tx_rx_vt_pr_slot::pat_rec_slot::SlottedPatRecorder,
        }
        impl eggplant::prelude::SingletonGetter for $name {
            type RetTy = eggplant::instances::tx_rx_vt_pr_slot::pat_rec_slot::SlottedPatRecorder;
            fn sgl()
            -> &'static eggplant::instances::tx_rx_vt_pr_slot::pat_rec_slot::SlottedPatRecorder
            {
                static INSTANCE: std::sync::OnceLock<$name> = std::sync::OnceLock::new();
                &INSTANCE
                    .get_or_init(|| -> $name {
                        Self {
                            tx: eggplant::instances::tx_rx_vt_pr_slot::pat_rec_slot::SlottedPatRecorder::new(),
                        }
                    })
                    .tx
            }
        }
    };
}
