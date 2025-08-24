# FPGA RS232 UART Loopback

This repository contains a VHDL-based UART loopback system designed specifically for the Altera Cyclone IV Board V3.0, featuring the Cyclone IV FPGA EP4CE6E22C8. The project implements a buffered UART interface using an internal FIFO, where every byte received through the RS-232 line is stored and then retransmitted back to the sender.

If you use a different development board or FPGA, some modifications and adjustments may be necessary to adapt the design to your hardware.

## Tools Used

- **Quartus Prime 20.1** – for synthesis, fitting, timing analysis, and programming the FPGA.
- **ModelSim** – for VHDL simulation and waveform analysis.

## Board Overview

The [Altera Cyclone IV Board V3.0](https://github.com/lucasmazzetto/Altera-Cyclone-IV-board-V3.0) includes:

- Cyclone IV EP4CE6E22C8 FPGA
- RS232 serial port interface

## Frame format

Each transmitted byte uses a simple, standard UART frame consisting of 1 start bit (logic 0), 8 data bits sent least-significant bit first, and 1 stop bit (logic 1). This design does not use parity bits, making the protocol straightforward and suitable for basic loopback testing and simple serial communication.


## Requirements

To properly test the UART loopback system, the following are required:

1. An FPGA board configured with a UART RS-232 interface.

2. A serial communication port (RS-232) available on the computer, or alternatively an RS-232 to USB adapter if no native port is present.

3. A computer with Python 3 installed, used to send and receive test data over the serial connection.

Once the hardware is ready, clone this repository and install the Python dependencies:

```bash
# Go to the scripts folder
cd fpga_rs232_uart/scripts

# Create a virtual environment for Python
python3 -m venv venv
source venv/bin/activate
python -m pip install --upgrade pip

# Install the dependencies
pip install -r requirements.txt
```

## Usage

1. Compile, synthesize, and upload the VHDL design to your FPGA board, using fpga_rs232_uart.vhd as the top-level entity.

2. Once the bitstream is loaded, connect the FPGA’s RS-232 UART interface to your computer. If your computer does not provide a native RS-232 port, you can use an RS-232 to USB adapter.

3. In the repository’s scripts folder, run the Python test script to validate the loopback:

```bash
# Go to the scripts folder
cd fpga_rs232_uart/scripts

# Run the python script
python uart_test.py
```

By default, the script uses ```/dev/ttyUSB0``` as the serial interface. Depending on your operating system and hardware, you may need to modify this path in the script.

The script will send all 256 possible byte values (0x00–0xFF) through the UART and check that each one is echoed back correctly. Any mismatches will be reported.

## Parameters

The following parameters can be modified in the VHDL code to adapt the system to different hardware configurations or application requirements:

- Clock Frequency (CLK_FREQ): 50 MHz (default).
- Baud Rate (BAUD_RATE): 115200 bps (default).
- FIFO Depth: 1024 entries (8-bit wide).


### Pin Assignments
| Signal          | Direction | Board Pin |
|-----------------|-----------|-----------|
| uart_rxd        | Input     |  PIN_115  |
| uart_txd        | Output    |  PIN_114  |

