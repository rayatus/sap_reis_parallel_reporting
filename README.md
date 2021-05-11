## HowTo... enable REIS parallel execution

### Preconditions

Ensure that all input and out parameters types are defined in SE11 (table types, estructures, ranges....)
- all parameters passed to REIS ABAP Class via ```CL_RECA_DATA_CONTAINER``` are related to a type in dictionary.
- resulting list also should be typed as table type in dictionary

If not, serialization/deserialization errors will occur.

### Steps

1. In REIS report add include ```zreca_reis_parallel```
2. Implement subroutine ```initialization_rep``` and in there execute ```PERFORM on_initialization_par.```
3. Implement subroutine ```at_selection_screen_output_rep``` and in there execute ```PERFORM on_selection_screen_output_par.```
4. Implement subroutine ```end_of_selection_rep``` and in there:
  1. Pass to ```CL_RECA_DATA_CONTAINER``` relevant selection filter with a dictionary based type
  2. Provide ```CL_RECA_DATA_CONTAINER``` to ```set_additional_param```
  3. Execute subroutine ```PERFORM execute_reis_report CHANGING go_data lo_param.``` where ```lo_param``` is the CL_RECA_DATA_CONTAINER with the relevant selection screen filters. Do not execute ```go_data-->complete_data( ).``` directly as it is executed from that subroutine.
5. Create a REIS data class inheriting from ```CL_REIS_DATA``` or any of its subclasses and implement interface ```zif_reca_reis_parallel_task```. Assign that REIS_DATA class to your REIS report.
  1. Implement method ```zif_reca_reis_parallel_task~get_filter_by_objectid``` to return a unique list of ID's that later on will be used to split the process into several Threads
  2. Implement method ```zif_reca_reis_parallel_task~set_filter_by_objectid``` to retrieve a list of unique ID's that should be taken into account while executing the data selection. This method will be executed per each Tread to ensure that each one only processes what was splitted in advance.
6.  Create an ABAP class implementing ```zcl_reca_reis_task_process```
  1. Implement mehtod ```get_reportid``` to return the ID of the REIS report that is going to be executed in parallel processing.
7. Add following configuration:
  1. via SM30 add new entry in ```v_tivcaimpl``` with following values:
  
  Interface|Subtype|ImplClass
  ---------|---------|---------
  |IF_RECA_TASK|<your_REIS_id>|<your_abap_class_implementing_zcl_reca_reis_task_process>|
  

