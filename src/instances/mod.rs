pub mod pat_rec;
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
        eggplant::basic_tx_rx_vt_pr!($tx_name);
        eggplant::basic_patttern_recorder!($pat_rec_name);
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
