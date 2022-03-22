INTERFACE zif_reca_reis_parallel_task
  PUBLIC .
  TYPES mtyp_d_objectid TYPE c LENGTH 28.
  TYPES mtyp_d_taskid TYPE guid_22.

  TYPES: BEGIN OF mtyp_s_taskid_objectid,
           traskid  TYPE mtyp_d_taskid,
           objectid TYPE mtyp_d_objectid,
         END   OF mtyp_s_taskid_objectid.
  TYPES mtyp_t_taskid_objectid TYPE STANDARD TABLE OF mtyp_s_taskid_objectid WITH DEFAULT KEY.
  TYPES mtyp_t_objectid TYPE STANDARD TABLE OF mtyp_d_objectid WITH DEFAULT KEY.

  METHODS set_filter_by_objectid importing it_objectid TYPE mtyp_t_objectid.
  METHODS get_filter_by_objectid exporting et_objectid TYPE mtyp_t_objectid.

ENDINTERFACE.
