
## Background

This application can be used to record, review, and analyze water quality sensor calibration data. Calibrations can be added from an SVMAQ XML file, or manually entered into the app. Calibration data is stored in a single file SQLite database, and can be retrieved to view or edit later. Currently, the application supports water temperature, specific conductance, turbidity, pH, and dissolved oxygen calibrations.

## Viewing/ editing calibrations

To view or edit an existing calibration, select the "View calibrations" menu item from the sidebar menu on the left. The menu can be opened or closed by clicking the menu button a top left.

### Finding and viewing a calibration

To find a calibration, select the parameter, serial number, and a date range. By default, the last year is selected. The "View data from" drop-down menu will show a list of dates of all calibrations matching the selected parameter, sensor, and date range. Select one of those dates to see data from that calibration.

### Making changes to a calibration 

To edit the calibration, select the "Edit" tab. Make any desired changes and click the "Update" button. Depending on the selected parameter, the pre-calibration and post-calibration readings are shown in separate tabs. To verify the changes were made correctly, go back to the "View" tab and click the "Refresh" button to update the calibration with the changes. A change can't be undone once its made, but the original data is saved to a file when a calibration is updated in case its needed, or something goes wrong. This backup data is overwritten any time another calibration is updated.

### Deleting a calibration

To delete a calibration, select it in the same way as if editing the calibration. Type the word "delete" in the text box next to the "Delete" button, and click "Delete." This is to avoid accidentally deleting a calibration. Deleting a calibration can't be undone.

## Adding a new calibration

New calibrations can be added manually or from an existing SVMAQ file that includes calibration data. You can access either option under the "New calibration" in the sidebar menu.

### From an SVMAQ file

#### Uploading a single file

Select "New calibration" > "From XML" from the sidebar menu. In the "Single" tab, click "Browse..." and use the file broswer to navigate to the XML file you want to upload. Once selected, a table of sensors and a message indicating what calibration data was found in the file will be displayed. To record the file to the database, click "Record file."  

#### Uploading multiple files

Select "New calibration" > "From XML" from the sidebar menu. In the "Batch" tab, click "Browse..." and use the file broswer to selec the XML Files you want to upload. You can control click to select multiple files, or hit Ctrl + A to select all the files in a folder. To record the files to the database, click "Record files." When files are succesfully written, messages will appear in the lower right corner. You will also see a message for any files that couldn't be written. 

Select 

### Manually

To manually enter calibration data, select "New calibration" > "Manual" from the sidebar menu. The format for entering data roughly mirrors the format of SVMAQ calibrations. Each parameter has its own tab. Enter any applicable data. Any parameters left blank won't be written to the database. To change the number of readings being entered, drag the "Number of readings" slider to the appropriate number. **Changing the slider position will clear any existing readings, so set it before entering any readings**. To record the data, select the "Record" tab, and click "Record".

## Downloading reports

### Probe history

The probe history report lists all the calibrations for a particular probe. The plot at the top shows the error over time for each standard, and indicates when the probe was re-calibrated. Select "Reports" > "Probe history" from the sidebar menu. Select the parameter from the "Parameter" drop-down, select the serial number from the "Probe serial number" drop down, and click "Get report".
  