# JT Pump Driver

Program output and visualize SIX sensor values: https://www.jobst-technologies.com/products/biosensors/
and to control a driver for CCP1 micropumps: https://www.jobst-technologies.com/products/microfluidics/

# Usage

To readout and visualize biosensorsensor data:
- Load a sensor definition file either using the corresponding button
  or by dropping it inot the program window.
  (For the expert option "Display values in nA" this is not necessary.)
- Click on the menu *Connection → SIX biosensors* and select the COM port of the SIX device.
- In the appeariing file dialog select a file in which the sonsor output should be stored.
- To change the appearance of the values chart, double-click on the different chart elements
  like the axes. To change the sensor channel names, double click on them.

To run pumps:
- Connect the pump driver to the PC.
- Click on the menu *Connection → Pump driver* and select the COM port of the pump driver.
- Set the properties you like and press *Run Pumps*.

To update or upload a new firmware:
- Connect the pump driver to the PC.
- Use the menu *Miscellaneous → Firmware Update*.
- Follow the steps provided by the software.

You can get the latest firmware version as binary from here: https://github.com/JobstTechnologies/JT-PumpDriver-Firmware/releases

# Copyrights

Copyright by Jobst Technologies.

This project uses the Arduino tool **bossac**: https://github.com/arduino/arduino-flash-tools

# Compilation

- Install the **Lazarus** IDE: https://www.lazarus-ide.org/.
- Open the file *JTDriverSensing.lpi* in Lazarus.
- Build the Lazarus project or run it.
