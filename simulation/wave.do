onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /uart_tb/clk
add wave -noupdate /uart_tb/rst
add wave -noupdate /uart_tb/data_stream_tx
add wave -noupdate /uart_tb/data_stream_tx_stb
add wave -noupdate /uart_tb/data_stream_tx_ack
add wave -noupdate /uart_tb/data_stream_rx
add wave -noupdate /uart_tb/data_stream_rx_stb
add wave -noupdate /uart_tb/tx
add wave -noupdate /uart_tb/rx
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {26756 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 181
configure wave -valuecolwidth 84
configure wave -justifyvalue left
configure wave -signalnamewidth 1
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ps
update
WaveRestoreZoom {0 ps} {268567 ps}
