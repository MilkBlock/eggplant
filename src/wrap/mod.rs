pub use derive_more;
mod func;
pub use func::*;
mod literal;
pub use literal::*;
mod evalue;
pub use evalue::*;
mod type_reg;
pub use type_reg::*;

mod wrap;
pub use wrap::*;

pub mod pat_rec;
pub use pat_rec::*;

pub mod tx;
pub mod tx_minimal;
pub mod tx_rx_vt;
pub mod tx_rx_vt_pr;
pub mod tx_vt;

pub mod rule;
pub use rule::*;

/// macro to quickly define a Transimitter with no version control
#[macro_export]
macro_rules! basic_tx_no_vt {
    ($name:ident) => {
        pub struct $name {
            tx: eggplant::wrap::tx::TxNoVT,
        }
        impl eggplant::prelude::SingletonGetter for $name {
            type RetTy = eggplant::wrap::tx::TxNoVT;
            fn sgl() -> &'static eggplant::wrap::tx::TxNoVT {
                static INSTANCE: std::sync::OnceLock<$name> = std::sync::OnceLock::new();
                &INSTANCE
                    .get_or_init(|| -> $name {
                        Self {
                            tx: eggplant::wrap::tx::TxNoVT::new(),
                        }
                    })
                    .tx
            }
        }
        impl eggplant::wrap::NonPatRecSgl for $name {}
    };
}
/// macro to quickly define a Transimitter with version control
#[macro_export]
macro_rules! basic_tx_vt {
    ($name:ident) => {
        pub struct $name {
            tx: eggplant::wrap::tx_vt::TxVT,
        }
        impl eggplant::prelude::SingletonGetter for $name {
            type RetTy = eggplant::wrap::tx_vt::TxVT;
            fn sgl() -> &'static eggplant::wrap::tx_vt::TxVT {
                static INSTANCE: std::sync::OnceLock<$name> = std::sync::OnceLock::new();
                &INSTANCE
                    .get_or_init(|| -> $name {
                        Self {
                            tx: eggplant::wrap::tx_vt::TxVT::new(),
                        }
                    })
                    .tx
            }
        }
        impl eggplant::wrap::NonPatRecSgl for $name {}
    };
}
/// macro to quickly define a minimal Transimitter
#[macro_export]
macro_rules! basic_tx_minimal {
    ($name:ident) => {
        pub struct $name {
            tx: eggplant::wrap::tx_minimal::TxMinimal,
        }
        impl eggplant::prelude::SingletonGetter for $name {
            type RetTy = eggplant::wrap::tx_minimal::TxMinimal;
            fn sgl() -> &'static eggplant::wrap::tx_minimal::TxMinimal {
                static INSTANCE: std::sync::OnceLock<$name> = std::sync::OnceLock::new();
                &INSTANCE
                    .get_or_init(|| -> $name {
                        Self {
                            tx: eggplant::wrap::tx_minimal::TxMinimal::new(),
                        }
                    })
                    .tx
            }
        }
        impl eggplant::wrap::NonPatRecSgl for $name {}
    };
}

#[macro_export]
macro_rules! basic_tx_rx_vt {
    ($name:ident) => {
        pub struct $name {
            tx: eggplant::wrap::tx_rx_vt::TxRxVT,
        }
        impl eggplant::prelude::SingletonGetter for $name {
            type RetTy = eggplant::wrap::tx_rx_vt::TxRxVT;
            fn sgl() -> &'static eggplant::wrap::tx_rx_vt::TxRxVT {
                static INSTANCE: std::sync::OnceLock<$name> = std::sync::OnceLock::new();
                &INSTANCE
                    .get_or_init(|| -> $name {
                        Self {
                            tx: eggplant::wrap::tx_rx_vt::TxRxVT::new(),
                        }
                    })
                    .tx
            }
        }
        impl eggplant::wrap::NonPatRecSgl for $name {}
    };
}

#[macro_export]
macro_rules! basic_tx_rx_vt_pr {
    ($name:ident) => {
        pub struct $name {
            tx: eggplant::wrap::tx_rx_vt_pr::TxRxVTPR,
        }
        impl eggplant::prelude::SingletonGetter for $name {
            type RetTy = eggplant::wrap::tx_rx_vt_pr::TxRxVTPR;
            fn sgl() -> &'static eggplant::wrap::tx_rx_vt_pr::TxRxVTPR {
                static INSTANCE: std::sync::OnceLock<$name> = std::sync::OnceLock::new();
                &INSTANCE
                    .get_or_init(|| -> $name {
                        Self {
                            tx: eggplant::wrap::tx_rx_vt_pr::TxRxVTPR::new(),
                        }
                    })
                    .tx
            }
        }
        impl eggplant::wrap::NonPatRecSgl for $name {}
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
    };
}

#[macro_export]
macro_rules! basic_patttern_recorder {
    ($name:ident) => {
        #[derive(Debug)]
        pub struct $name {
            tx: eggplant::wrap::PatRecorder,
        }
        impl eggplant::prelude::SingletonGetter for $name {
            type RetTy = eggplant::wrap::PatRecorder;
            fn sgl() -> &'static eggplant::wrap::pat_rec::PatRecorder {
                static INSTANCE: std::sync::OnceLock<$name> = std::sync::OnceLock::new();
                &INSTANCE
                    .get_or_init(|| -> $name {
                        Self {
                            tx: eggplant::wrap::pat_rec::PatRecorder::new(),
                        }
                    })
                    .tx
            }
        }
    };
}
