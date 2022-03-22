*&---------------------------------------------------------------------*
*& Report zreisdmoparallel
*&---------------------------------------------------------------------*
*& Report to demonstrate REIS execution in parallel tasks
*&---------------------------------------------------------------------*
REPORT zreisdmoparallel.

*=======================================================================
TABLES:
*=======================================================================
  vicncn.
*=======================================================================
CONSTANTS:
*=======================================================================
  gc_reportid TYPE reisrepid VALUE zcl_reca_reis_demo_paralleltsk=>mc_reportid.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME.
SELECT-OPTIONS: s_bukrs FOR vicncn-bukrs,
                s_cnnr  FOR vicncn-recnnr.
SELECTION-SCREEN END OF BLOCK a.

*=======================================================================
INCLUDE:
*=======================================================================
  ifreis_ca_global,
  ifreis_ca_selscr,
  ifreca_parallel_screen,
  ifreis_ca_events,
  ifreis_ca_forms,
  zreca_reis_parallel.

*=======================================================================
FORM initialization_rep ##CALLED.
*=======================================================================
  PERFORM on_initialization_par.
ENDFORM.

*=======================================================================
FORM at_selection_screen_output_rep ##CALLED.
*=======================================================================
  PERFORM on_selection_screen_output_par.
ENDFORM.

*=======================================================================
FORM end_of_selection_rep ##CALLED.
*=======================================================================
  DATA lr_recnnr TYPE re_t_rsorecnnr.
  DATA lr_bukrs  TYPE tms_t_bukrs_range.

  "To ensure that parameters al Dictionary type based
  lr_recnnr[] = s_cnnr[].
  lr_bukrs[] = s_bukrs[].

  DATA(lo_param) = NEW cl_reca_data_container(  ).

  lo_param->set( id_id = 'BUKRS'  ix_data = lr_bukrs[] ).
  lo_param->set( id_id = 'RECNNR' ix_data = lr_recnnr[] ).

  go_data->set_additional_param( lo_param ).
  PERFORM execute_reis_report USING go_data lo_param.

ENDFORM.
