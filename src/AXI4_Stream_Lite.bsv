package AXI4_Stream_Lite;

import GetPut :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import Connectable :: *;
import Clocks :: *;

// Project specific

/*
=============
	Types
=============
*/

typedef struct {
		Bit#(datawidth) data;
	} AXI4_Stream_Lite_Pkg#(numeric type datawidth) deriving(Bits, Eq, FShow);

typedef AXI4_Stream_Lite_Pkg#(32) AXI4_Stream_Lite_Pkg_32;
typedef AXI4_Stream_Lite_Pkg#(64) AXI4_Stream_Lite_Pkg_64;
typedef AXI4_Stream_Lite_Pkg#(128) AXI4_Stream_Lite_Pkg_128;
typedef AXI4_Stream_Lite_Pkg#(256) AXI4_Stream_Lite_Pkg_256;
typedef AXI4_Stream_Lite_Pkg#(512) AXI4_Stream_Lite_Pkg_512;

/*
========================
	AXI 4 Stream Read
========================
*/

(* always_ready, always_enabled *)
interface AXI4_Stream_Lite_Rd_Fab#(numeric type datawidth);
  method Bool tready;
  (*prefix=""*)method Action ptvalid((*port="tvalid"*) Bool tvalid);
  (*prefix=""*)method Action ptdata((*port="tdata"*)Bit#(datawidth) data);
endinterface

interface AXI4_Stream_Lite_Rd#(numeric type datawidth);
  (* prefix="" *)
  interface AXI4_Stream_Lite_Rd_Fab#(datawidth) fab;
  interface Get#(AXI4_Stream_Lite_Pkg#(datawidth)) pkg;
endinterface

module mkAXI4_Stream_Lite_Rd#(Integer bufferSize)(AXI4_Stream_Lite_Rd#(datawidth));

    let isRst <- isResetAsserted();

	Wire#(Bool) 					  tvalidIn <- mkBypassWire();
	Wire#(Bit#(datawidth)) 			  dataIn <- mkBypassWire();

	FIFOF#(AXI4_Stream_Lite_Pkg#(datawidth)) in <- mkSizedFIFOF(bufferSize);
	if(bufferSize == 1)
		in <- mkPipelineFIFOF();
	if(bufferSize == 2)
		in <- mkFIFOF();

	rule writeFIFO if(!isRst && tvalidIn && in.notFull());
		AXI4_Stream_Lite_Pkg#(datawidth) s;
		s.data = dataIn;
		in.enq(s);
	endrule

	interface Get pkg = toGet(in);

	interface AXI4_Stream_Lite_Rd_Fab fab;
		interface tready = !isRst && in.notFull();
		interface ptvalid = tvalidIn._write;
		interface ptdata = dataIn._write;
	endinterface
endmodule

/*
========================
	AXI 4 Stream Write
========================
*/

(* always_ready, always_enabled *)
interface AXI4_Stream_Lite_Wr_Fab#(numeric type datawidth);
  method Bool tvalid;
  (*prefix=""*)method Action ptready((*port="tready"*) Bool tr);
  method Bit#(datawidth) tdata;
endinterface

interface AXI4_Stream_Lite_Wr#(numeric type datawidth);
  (* prefix="" *)
  interface AXI4_Stream_Lite_Wr_Fab#(datawidth) fab;
  interface Put#(AXI4_Stream_Lite_Pkg#(datawidth)) pkg;
endinterface

module mkAXI4_Stream_Lite_Wr#(Integer bufferSize)(AXI4_Stream_Lite_Wr#(datawidth));

    let isRst <- isResetAsserted();

	FIFOF#(AXI4_Stream_Lite_Pkg#(datawidth)) out <- mkSizedFIFOF(bufferSize);
	if(bufferSize == 1)
		out <- mkPipelineFIFOF();
	if(bufferSize == 2)
		out <- mkFIFOF();

	Wire#(Bool) treadyIn <- mkBypassWire;
	Wire#(Bool) tvalidOut <- mkDWire(False);
	Wire#(Bit#(datawidth)) tdataOut <- mkDWire(unpack(0));

	rule deqFIFO if(!isRst && treadyIn && out.notEmpty());
		out.deq();
	endrule

	rule writeOutputs;
		tdataOut <= out.first().data();
	endrule

	interface AXI4_Stream_Lite_Wr_Fab fab;
		interface tvalid = !isRst && out.notEmpty();
		interface ptready = treadyIn._write();
		interface tdata = tdataOut;
	endinterface

	interface Put pkg = toPut(out);
endmodule
/*
========================
DUMMY
========================
*/
module mkAXI4_Stream_Lite_Wr_Dummy(AXI4_Stream_Lite_Wr#(datawidth));
	interface AXI4_Stream_Lite_Wr_Fab fab;
		interface tvalid = False;
		interface tdata = 0;
		method Action ptready(Bool tr);
		endmethod
	endinterface
	interface Put pkg;
		method Action put(AXI4_Stream_Lite_Pkg#(datawidth) p);
		endmethod
	endinterface
endmodule

module mkAXI4_Stream_Lite_Rd_Dummy(AXI4_Stream_Lite_Rd#(datawidth));
	interface AXI4_Stream_Lite_Rd_Fab fab;
		interface tready = False;
  		method Action ptvalid(Bool tvalid);
  		endmethod
  		method Action ptdata(Bit#(datawidth) data);
  		endmethod
	endinterface

	interface Get pkg;
		method ActionValue#(AXI4_Stream_Lite_Pkg#(datawidth)) get();
			return unpack(0);
		endmethod
	endinterface
endmodule

/*
========================
	Connectable
========================
*/

instance Connectable#(AXI4_Stream_Lite_Wr_Fab#(datawidth), AXI4_Stream_Lite_Rd_Fab#(datawidth));
	module mkConnection#(AXI4_Stream_Lite_Wr_Fab#(datawidth) wr, AXI4_Stream_Lite_Rd_Fab#(datawidth) rd)(Empty);

		rule forward1;
			wr.ptready(rd.tready());
		endrule
		rule forward2;
			rd.ptvalid(wr.tvalid());
		endrule
		rule forward3;
			rd.ptdata(wr.tdata());
		endrule

	endmodule
endinstance

endpackage
