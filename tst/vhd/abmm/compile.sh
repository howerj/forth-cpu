#/bin/bash

./clean.sh

if
  ghdl -a abmm_multiplexer.vhd abmm.vhd abmm_tb.vhd;
then
  echo "Analysis complete!";
else
  echo "Analysis failed.";
  exit 1;
fi

if
  ghdl -e abmm_tb;
then
  echo "Executable built!";
else
  echo "Executable failed to build.";
  exit 1;
fi

if
  ghdl -r abmm_tb --wave=test_bench.ghw;
then
  echo "Running VHDL simulation complete!";
else
  echo "Simulation failed."
  exit 1;
fi
