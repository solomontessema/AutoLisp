# Before You Begin

1. Open the sample drawing titled **"Sample Drawing.dwg"** in AutoCAD 2018 or any later version.
2. Disregard any warnings about **"Missing SHX Files."**
3. Load the Lisp files `Station.lsp` and `TM.lsp` using the AutoCAD command `APPLOAD`.

## Instructions for Using TM.lsp (AutoLisp for Creating Station Marks on a Baseline)

1. Activate the command by typing `TM` in the command line.
2. When prompted, select the `UP` option from the displayed choices.
3. Enter the initial number for station marking. For this example, input `1`.
4. Choose the object to measure by selecting the purple baseline depicted in the drawing.

**Expected Result:** Station marks will be systematically placed along the selected baseline.

## Instructions for Using Station.lsp (AutoLisp for Placing Street Objects Relative to a Specified Baseline)

1. Initiate the command by typing `BL`, then select the purple baseline.
2. Begin the object placement process by typing `PlaceObject`.
3. When prompted to specify an object, input `POLE` as an example.
4. Upon request to specify a station, input `145` as an illustrative station number.
5. When asked to specify an offset, type `10` to define the distance from the baseline.

**Expected Result:** A "Pole" symbol will be accurately positioned at station 145 with a 10-unit offset from the baseline.

---

**Copyright**

Solomon Tessema

**Revised:** March 2024
 