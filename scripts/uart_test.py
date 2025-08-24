import serial
import time

# Configure your UART port
ser = serial.Serial('/dev/ttyUSB0', 115200, timeout=1)

try:
    errors = 0  # count mismatches
    for byte_out in range(256):
        # Send the byte
        ser.write(bytes([byte_out]))

        # Read the response (expecting 1 byte)
        response = ser.read(1)

        if len(response) == 0:
            print(f"Timeout: sent 0x{byte_out:02X}, no response")
            errors += 1
            continue

        byte_in = response[0]

        # Compare
        if byte_in != byte_out:
            print(f"Mismatch: sent 0x{byte_out:02X}, received 0x{byte_in:02X}")
            errors += 1
        else:
            print(f"OK: 0x{byte_out:02X}")

    print(f"\nValidation complete: {256-errors} passed, {errors} errors")

finally:
    ser.close()
