#Assignment Attempt Explorer

This tool seeks to help people look at what a student's attempt at their 
assignments may say about that student's subsequent likelihood of passing 
or getting a specific grade in a course. If you use this system in research
please cite it.

## Invocation

To run the system you need to run the general.R script. This will give the URL
of a locally hosted website which can be opened to see the results. Since this
uses shiny the webpage will only be visible while the script continues to run.

## Command line options

The minimal invocation instruction is

```bash
./general.sh --input inputFile.csv
```

There are a number of additional arguments that  can also be passed to the
system. Passing `--help` will print the help.

## Expected file format

The input file must be a CSV file, with each row representing a single student.
The first row in the file must be a header row. The following columns are
expected:
   * ID - A unique identifier for each student
   * Final - The students final mark in the course. Marks outside of the set 
     {A+,A,A-,B+,B,B-,C+,C,C-,D,E} will be ignored. F is used in the output to 
     represent a general fail grade (D or E).
   * a1..aN - A set of columns all with a fixed prefix ('a' by default, but 
     customised using the aregex argument) to indicate a mark for each 
     assignment. Each column is numbered with the assignment number. It is 
     assumed that assignments are all numbered. 
 
All other columns are ignored (but may be present).

## Dependencies

This script relies on having R installed, and Rscript being on the system PATH.
You will require a browser with JavaScript enabled to view the results. 
It uses the following R libraries:
   * magrittr
   * dplyr
   * ggplot2
   * reshape2
   * data.table
   * shiny
   * argparser
