use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

tx_rx_vt_pr!(MyTxPythonArrayOptimize, MyPatRecPythonArrayOptimize);

include!("../generated/python_array_optimize.rs");
