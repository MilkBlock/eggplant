use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

tx_rx_vt_pr!(MyTxEggccExtraction, MyPatRecEggccExtraction);

include!("../generated/eggcc_extraction.rs");
