///////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1995/2010 Xilinx, Inc.
// All Right Reserved.
///////////////////////////////////////////////////////////////////////////////
// This version is a simulation stub.

module BSCANE2 (
  output CAPTURE,
  output DRCK,
  output RESET,
  output RUNTEST,
  output SEL,
  output SHIFT,
  output TCK,
  output TDI,
  output TMS,
  output UPDATE,

  input TDO
);

  parameter DISABLE_JTAG = "FALSE";
  parameter integer JTAG_CHAIN = 1;

  supply0 CAPTURE;
  supply0 DRCK;
  supply0 RESET;
  supply0 RUNTEST;
  supply0 SEL;
  supply0 SHIFT;
  supply0 TCK;
  supply0 TDI;
  supply0 TMS;
  supply0 UPDATE;

endmodule
