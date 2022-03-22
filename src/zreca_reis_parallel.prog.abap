*&---------------------------------------------------------------------*
*& Include zreca_reis_parallel
*&---------------------------------------------------------------------*

**=======================================================================
*FORM initialization_rep ##CALLED.
**=======================================================================
*  PERFORM on_initialization_par.
*ENDFORM.                    "initialization_rep
*
**=======================================================================
*FORM at_selection_screen_output_rep ##CALLED.
**=======================================================================
*  PERFORM on_selection_screen_output_par.
*ENDFORM.

*=======================================================================
FORM execute_reis_report
    USING
        io_data      TYPE REF TO cl_reis_data
        io_param     TYPE REF TO cl_reca_data_container ##CALLED.
*=======================================================================
  IF p_para = abap_true.
    PERFORM execute_parallel USING io_data io_param.
  ELSE.
    CALL METHOD io_data->complete_data( ).
  ENDIF.

ENDFORM.

*=======================================================================
FORM execute_parallel
    USING
        io_data  TYPE REF TO cl_reis_data
        io_param TYPE REF TO cl_reca_data_container ##CALLED.
*=======================================================================
  DATA io_task_handler TYPE REF TO if_reca_task_handler.

  PERFORM init_tasks USING    io_data
                              io_param
                     CHANGING io_task_handler.
  IF io_task_handler IS BOUND.
    io_task_handler->start_tasks(  ).
    PERFORM collect_results USING io_data io_task_handler.
  ENDIF.
ENDFORM.

*=======================================================================
FORM init_tasks
    USING
        io_data  TYPE REF TO cl_reis_data
        io_param TYPE REF TO cl_reca_data_container
    CHANGING
        io_task_handler TYPE REF TO if_reca_task_handler ##CALLED.
*=======================================================================
  DATA ls_task_param TYPE recataskparam.
  DATA ld_step       TYPE i.
  DATA ld_rest       TYPE i.
  DATA lt_object     TYPE re_t_taskobjid.
  DATA lo_task_impl  TYPE REF TO zcl_reca_reis_task_process.

  DATA(lo_msglist) = cf_reca_message_list=>create(  ).

  ls_task_param-servergrp = p_sgrp.
  ls_task_param-maxtasks  = p_task.

  IF ls_task_param-maxtasks IS INITIAL.
    ls_task_param-maxtasks = 1.
  ENDIF.

  "Just for debugging purposses
  DATA(lf_debug) = abap_false.
  IF lf_debug = abap_true.
    ls_task_param-singletaskmode = abap_true.
  ENDIF.

  "Ensure that parameters are serializable
  zcl_reca_reis_task_process=>check_serialization_possible( io_msglist = lo_msglist io_container = io_param  ).
  IF lo_msglist->has_messages_of_msgty( 'E' ) = abap_true.
    lo_msglist->get_last_message( EXPORTING id_msgty   = 'E'
                                   IMPORTING es_message = DATA(ls_message) ).
    IF ls_message IS NOT INITIAL.
      MESSAGE ID      ls_message-msgid
              TYPE    ls_message-msgty
              NUMBER  ls_message-msgno
              WITH    ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.
    ENDIF.
  ENDIF.

  "Ensure that custo is correctly filled
  cl_recac_interface_impl=>get_detail( EXPORTING  id_implifname  = 'IF_RECA_TASK'
                                                  id_implsubtype = CONV #( io_data->md_reportid )
                                                  id_require     = 'K'
                                       RECEIVING  rs_detail      = DATA(ls_task_impl_detail)
                                       EXCEPTIONS OTHERS         = 999 ).
  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID      sy-msgid
            TYPE    sy-msgty
            NUMBER  sy-msgno
            WITH    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  TRY.
      CAST zif_reca_reis_parallel_task( io_data )->get_filter_by_objectid( IMPORTING et_objectid = DATA(lt_objectid) ).
    CATCH cx_sy_move_cast_error INTO DATA(lx_move_cast_error).
      "Reis report does not implements required interface
      MESSAGE lx_move_cast_error->get_text(  ) TYPE 'E'.
  ENDTRY.

  IF p_locl = abap_true.
    PERFORM par_loc_server_create IN PROGRAM (sy-repid) IF FOUND
        CHANGING ls_task_param-servergrp.
  ENDIF.

  "Split the total objects to be processed into tasks
  ld_step = lines( lt_objectid ) DIV ls_task_param-maxtasks.
  ld_rest = lines( lt_objectid ) MOD ls_task_param-maxtasks.

  IF ld_rest <> 0.
    ADD 1 TO ld_step.
  ENDIF.
  IF ld_step IS INITIAL.
    ld_step = 1.
  ENDIF.

  IF p_objtsk IS NOT INITIAL AND p_objtsk < ld_step.
    ld_step = p_objtsk.
  ENDIF.

  CREATE OBJECT lo_task_impl TYPE (ls_task_impl_detail-implclname).

  LOOP AT lt_objectid INTO DATA(ls_objectid).
    IF sy-tabix MOD ld_step = 1 OR ld_step = 1.
      "New task
      cl_reca_guid=>guid_create( IMPORTING ed_guid_22 = DATA(ld_taskid) ).
    ENDIF.

    DATA(ls_object) =  lo_task_impl->taskid_and_objectid_to_object( id_objectid  = ls_objectid
                                                                    id_taskid    = ld_taskid ).

    INSERT ls_object INTO TABLE lt_object.
  ENDLOOP.

  io_task_handler =  cf_reca_task_handler=>create( EXPORTING id_tasktype   = CONV #( lo_task_impl->get_reportid(  ) )
                                                             is_task_param = ls_task_param
                                                             io_param      = io_param
                                                             it_object     = lt_object ).

ENDFORM.

*=======================================================================
FORM collect_results
    USING
        io_data         TYPE REF TO cl_reis_data
        io_task_handler TYPE REF TO if_reca_task_handler .
*=======================================================================
  FIELD-SYMBOLS <lt_list> TYPE ANY TABLE.
  DATA ld_string TYPE string.

  DATA(lo_data_container) = NEW cl_reca_data_container(  ).

  ASSIGN io_data->mr_list->* TO <lt_list>.

  LOOP AT io_task_handler->mt_return ASSIGNING FIELD-SYMBOL(<ls_task_return>).

    CALL TRANSFORMATION id SOURCE XML <ls_task_return>-task_return_xml
                           RESULT  iobj  = lo_data_container
                           OPTIONS clear = 'all'.

    lo_data_container->get_list( IMPORTING et_list = DATA(lt_container_list) ).
    LOOP AT lt_container_list INTO DATA(ls_container_list)
                              WHERE id CS zcl_reca_reis_task_process=>mc_container-resulting_list.

      lo_data_container->get( EXPORTING id_id = ls_container_list-id
                              IMPORTING ex_data = ld_string ).
      INSERT INITIAL LINE INTO TABLE <lt_list> ASSIGNING FIELD-SYMBOL(<ls_list>).

      cl_abap_container_utilities=>read_container_c( EXPORTING im_container = ld_string
                                                     IMPORTING ex_value     = <ls_list> ).

    ENDLOOP.

  ENDLOOP.

  SORT <lt_list>.

  io_data->mo_msglist->add_from_instance( io_task_handler->mo_msglist ).


ENDFORM.
