# JT Pump Driver

Program output and visualize SIX sensor values: https://www.jobst-technologies.com/products/biosensors/
and to control a driver for CCP micropumps: https://www.jobst-technologies.com/products/microfluidics/

# Usage

## SIX Biosensors

To readout and visualize biosensorsensor data:
- Load a sensor definition file either using the corresponding button
  or by dropping it inot the program window.
  (For the expert option "Display values in nA" this is not necessary.)
- Click on the menu *Connection → SIX Biosensors* and select the ID or the SIX device.
- In the appearing file dialog select a file in which the sonsor output should be stored.
- To change the appearance of the values chart, double-click on the different chart elements
  like the axes, legend etc. To change the sensor channel names, double click on them.
  
To (re-)calibrate a biosensor:
- Load a sensor definition file.
- Expose the sensor to a solution with a known glucose or lactate concentration.
- Wait until the sensor signal is stable.
- Press in the values chart the button *Calibrate*.
- Select the date range in the chart which shows the stable sensor signal by left-click-dragging.
  (You can repeat the selection process as often as you need.
   By click-dragging on the border of the selected area the area can be changed too.)
- When the selection is done, press the button *Calibrate* again.
- In the appearing dialog, select the calibration substance and the channel that measures the
  known concentration.
- After clicking OK, select a name for a definition file to store the new calibration data.
  (You must choose a new name since existing file must not be overwritten.)
- The new definition file be be loaded immediately.

## CPP Pumps

To run pumps:
- Connect the pump driver to the PC.
- Click on the menu *Connection → Pump Driver* and select the COM port of the pump driver.
- Either
 - set the properties you like
 - Or load an action file via the menu *File → Load Pump Action* or by dropping a file into
   the program while the tab *Pump Control* is open.
- Press *Run Pumps*.

To save a pump action:
- Only when you have a pump action file loaded:
 - Click the button *Generate Command* to unlock its settings.
- Set the parameters in the different steps and then use the menu
  *File → Save Pump Action*.

To update or upload a new firmware to a pump driver:
- Connect the pump driver to the PC.
- Use the menu *Miscellaneous → Firmware Update*.
- Follow the steps provided by the software.

You can get the latest firmware version as binary from here: https://github.com/JobstTechnologies/JT-PumpDriver-Firmware/releases

# Copyrights

Copyright by Jobst Technologies.

This project uses the Arduino tool **bossac**: https://github.com/arduino/arduino-flash-tools

# Compilation

- Install the **Lazarus** IDE: https://www.lazarus-ide.org/.
  Note: you must use Lazarus 2.2.0 or newer!
- Open the file *JTDriverSensing.lpi* in Lazarus.
- Build the Lazarus project or run it.
