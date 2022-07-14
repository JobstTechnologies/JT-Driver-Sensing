# JT Pump Driver

Program output and visualize SIX sensor values: https://www.jobst-technologies.com/products/biosensors/
and to control a driver for CCP micropumps: https://www.jobst-technologies.com/products/microfluidics/

# Usage

## SIX Biosensors

### Measuring Live Data

To readout and visualize biosensorsensor data:
- Load a sensor definition file either by
  - using the corresponding button.
  - dropping it into the program window when the tab *General* is open.
- Click on the button *Connect SIX* and select the ID of the SIX device.
- If no sensor definition file was loaded, you will prompted to do so.</br>
  (This can be canceled to work without a definition file.)
- In the appearing file dialog select a file in which the sensor output should be stored.
- To change the appearance of the values chart, double-click on the different chart elements
  like the axes, legend etc. To change the sensor channel names, double click on them in the
  tab *General*.
- To add notes in the chart, Ctrl + click on a datapoint. To modify or delete an existing note
  Ctrl + click on its datapoint.

### Calibration

To manually (re-)calibrate a biosensor:
- Load a sensor definition file.
- Expose the sensor to a solution with a known substance concentration.
- Wait until the sensor signal is stable.
- Press in the values chart the button *Calibrate*.
- Select the date range in the chart which shows the stable sensor signal by left-click-dragging.</br>
  (You can repeat the selection process as often as you need.</br>
   By click-dragging on the border of the selected area the area can be changed too.)
- When the selection is done, press the button *Calibrate* again.
- In the appearing dialog, select the calibration substance and the channel that measures the</br>
  known concentration.
- After clicking OK, select a name for a definition file to store the new calibration data.</br>
  (You must choose a new name since existing file must not be overwritten.)
- The new definition file will be loaded immediately.

To (re-)calibrate a biosensor automatically after a certain action step:
- Connect to a SIX and load a sensor definition file.
- In the tab *Action Control* check the option *Use Calibration*.
- This will dispay additional settings to specify after what action step the calibration should be</br>
  performed, on what substances and what substance concentrations.

### Diplaying Existing Measurements

Existing measurements can be displayed either by:
- using the menu *File → Load Sensor Data*.
- dropping a data file into the program window when the tab *SIX Values* is open.

## CP Pumps and Actions

To run pumps (actions like calibration can also be done without a pump driver):
- Connect the pump driver to the PC.
- Click on the button *Connect Driver* and select the COM port of the pump driver.
- Either
  - set the properties you like.
  - load an action file via the menu *File → Load Action File* or by dropping a file into
   the program while the tab *Action Control* is open.
- Press *Run Action*.

To save a pump action:
- Only necessary when you have a pump action file loaded:
  - Click the button *Generate Command* to unlock its settings.
- Set the parameters in the different steps and then use the menu
  *File → Save Action File*.

### Pump driver Firmware Update

To update or upload a new firmware to a pump driver:
- Connect the pump driver to the PC.
- Use the menu *Miscellaneous → Pump Driver Firmware Update*.
- Follow the steps provided by the software.

You can get the latest firmware version as binary from [here](https://github.com/JobstTechnologies/JT-PumpDriver-Firmware/releases/latest).

# Copyrights

Copyright by Jobst Technologies.

This project uses the Arduino tool [**bossac**](https://github.com/arduino/arduino-flash-tools).

# Compilation

- Install the [**Lazarus** IDE](https://www.lazarus-ide.org/).</br>
  Note: You must use Lazarus 2.2.0 or newer.
- Only for the first run of Lazarus:
  - Open the menu *Package → Online Package Manager* and install there the packages **LazSerial** and **Synapse**.
- Open the file *JTDriverSensing.lpi* in Lazarus.
- Build the Lazarus project or run it.
