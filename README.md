## HowTo... enable REIS parallel execution

### Preconditions

Ensure that all input and out parameters types are defined in SE11 (table types, estructures, ranges....)
- all parameters passed to REIS ABAP Class via CL_RECA_DATA_CONTAINER are related to a type in dictionary.
- resulting list also should be typed as table type in dictionary

If not, serialization/deserialization errors will occur.

### Steps

1. In REIS report add include ```IFRECA_PARALLEL_SCREEN```.
```abap 
INCLUDE:
  ifreis_ca_selscr,
  ifreca_parallel_screen,
  ifreis_ca_events,
  ifreis_ca_forms
```  
2. In report initialization event execute ```ON_INITIALIZATION_PAR``` subroutine.
```abap
*=======================================================================
FORM initialization_rep ##CALLED.
*=======================================================================
  PERFORM on_initialization_par.
ENDFORM.                    "initialization_rep
```  
3. In report output event execute ```ON_SELECTION_SCREEN_OUTPUT_PAR``` subroutine.
```abap
*=======================================================================
FORM at_selection_screen_output_rep ##CALLED.
*=======================================================================
  PERFORM on_selection_screen_output_par.
ENDFORM.
```  
4. Add global definition of ```IF_RECA_TASK_HANDLER``` to your REIS report.
```abap
*=======================================================================
data:
*=======================================================================
  go_task_handler type ref to if_reca_task_handler.
```  
5. Add folowing texts to selection screen fields.

| Text field | Label |
|---|---|
| P_SGRP | Server group |
| P_TASK | Max Tasks |
| P_PARA | Parallel execution |
| P_OBJTSK | Objects per task |
| P_LOCL | Local server |
| PAR | Parallel execution |

6. In start-of-selection event execute either subroutine ```EXECUTE_PARALLEL``` or ```COMPLETE_DATA``` method depending on if execution should be in parallel or not. Remember to replace:
* ```<your_reis_class with>``` with your REIS DATA class
* ```<your_method_that_returns_all_selectd_obects_to_calculate>``` the method that will allow you to retrieve the first group of data that starting from there will originate the resulting list (i.e. list of all contracts, list of financial documents to be analyzed, etc...).
* Ensure all data added into ```CL_RECA_DATA_CONTAINER``` are dictionary based typed (talbe types, ranges, data elements, ...)

```abap
*=======================================================================
FORM start_of_selection_rep ##CALLED.
*=======================================================================
  DATA lt_bukrs TYPE re_t_rsobukrs.
  DATA lt_be    TYPE re_t_rsoswenr.
  DATA lt_ro    TYPE re_t_rsosmenr.
  DATA lt_mngr  TYPE zttre_so_manageresp.
  DATA lt_pspnr TYPE zttre_so_pep.
  DATA lt_pspid TYPE zttre_so_project.
  DATA lt_nego  TYPE zttre_so_negosinmu.
  DATA lt_saknr TYPE zttre_so_saknr.

  DATA(lo_param) = NEW cl_reca_data_container( ).

  "==========================================================
  "Ensure that all filter criteria are typed as table types
  "defined in dictionary
  "==========================================================
  lt_bukrs = so_bukrs[].
  lt_mngr  = so_mngr[].
  lt_be    = so_be[].
  lt_ro    = so_ro[].
  lt_pspnr = so_pspnr[].
  lt_pspid = so_pspid[].
  lt_nego  = so_nego[].
  lt_saknr = so_saknr[].


  lo_param->add( id_id = 'P_GRID'   ix_data = p_grid ).
  lo_param->add( id_id = 'P_VARI'   ix_data = p_vari ).
  lo_param->add( id_id = 'P_TITLE'   ix_data = p_title ).
  lo_param->add( id_id = 'SO_BUKRS' ix_data = lt_bukrs[] ).
  lo_param->add( id_id = 'PA_DT_FR' ix_data = pa_dt_fr ).
  lo_param->add( id_id = 'PA_DT_TO' ix_data = pa_dt_to ).
  lo_param->add( id_id = 'SO_PSPID' ix_data = lt_pspid[] ).
  lo_param->add( id_id = 'SO_PSPNR' ix_data = lt_pspnr[] ).
  lo_param->add( id_id = 'SO_MNGR'  ix_data = lt_mngr[] ).
  lo_param->add( id_id = 'SO_NEGO'  ix_data = lt_nego[] ).
  lo_param->add( id_id = 'SO_BE'    ix_data = lt_be[] ).
  lo_param->add( id_id = 'SO_RO'    ix_data = lt_ro[] ).
  lo_param->add( id_id = 'SO_SAKNR' ix_data = lt_saknr[] ).
  lo_param->add( id_id = 'PA_CUREX' ix_data = pa_curex ).
  lo_param->add( id_id = 'PARALLEL' ix_data = p_para ).

  go_data->set_additional_param( lo_param ).

  IF p_para = abap_true.
    PERFORM execute_parallel USING lo_param.
  ELSE.
    CALL METHOD go_data->complete_data( ).
  ENDIF.
ENDFORM.                    "start_of_selection_rep

*=======================================================================
FORM execute_parallel USING io_param TYPE REF TO cl_reca_data_container.
*=======================================================================
  PERFORM init_tasks USING io_param.
  IF go_task_handler IS BOUND.
    go_task_handler->start_tasks(  ).
    PERFORM collect_results.
  ENDIF.
ENDFORM.

*=======================================================================
FORM init_tasks USING io_param TYPE REF TO cl_reca_data_container .
*=======================================================================
  DATA ls_task_param TYPE recataskparam.
  DATA ld_step TYPE i.
  DATA ld_rest TYPE i.
  DATA lt_object TYPE re_t_taskobjid.

  ls_task_param-servergrp = p_sgrp.
  ls_task_param-maxtasks  = p_task.
  IF ls_task_param-maxtasks IS INITIAL.
    ls_task_param-maxtasks = 1.
  ENDIF.

  CAST <your_reis_class>( go_data )-><your_method_that_returns_all_selectd_obects_to_calculate>( 
    IMPORTING et_total_costs = DATA(lt_objects_calculated) ).

  IF p_locl = abap_true.
    PERFORM par_loc_server_create
    IN PROGRAM (sy-repid) IF FOUND
    CHANGING ls_task_param-servergrp.
  ENDIF.

  "Split the total objects to be processed into tasks
  ld_step = lines( lt_objects_calculated ) DIV ls_task_param-maxtasks.
  ld_rest = lines( lt_objects_calculated ) MOD ls_task_param-maxtasks.

  IF ld_rest <> 0.
    ADD 1 TO ld_step.
  ENDIF.
  IF ld_step IS INITIAL.
    ld_step = 1.
  ENDIF.

  IF p_objtsk IS NOT INITIAL AND p_objtsk < ld_step.
    ld_step = p_objtsk.
  ENDIF.

  LOOP AT lt_objects_calculated INTO DATA(ls_object_calculated).
    IF sy-tabix MOD ld_step = 1 OR ld_step = 1.
      "New task
      cl_reca_guid=>guid_create( IMPORTING ed_guid_22 = DATA(ld_taskid) ).
    ENDIF.

    DATA(ls_object) = zre_cl_reis_opex_task=>fidockey_to_parallel_key( is_object = ls_object_calculated
                                                                       id_taskid = CONV #( ld_taskid ) ).

    INSERT ls_object INTO TABLE lt_object.
  ENDLOOP.

  go_task_handler =  cf_reca_task_handler=>create( EXPORTING id_tasktype   = CONV #( gc_reportid )
                                                             is_task_param = ls_task_param
                                                             io_param      = io_param
                                                             it_object     = lt_object ).

ENDFORM.

*=======================================================================
FORM collect_results.
*=======================================================================
  DATA lo_data_container TYPE REF TO cl_reca_data_container.
  DATA lt_messages TYPE bapirettab.
  DATA lt_messages_all TYPE bapirettab .
  DATA lt_list TYPE zttres_reis_opex_l.
  DATA lt_list_all TYPE zttres_reis_opex_l.

  CREATE OBJECT lo_data_container.

  LOOP AT go_task_handler->mt_return ASSIGNING FIELD-SYMBOL(<ls_task_return>).

    CALL TRANSFORMATION id SOURCE XML <ls_task_return>-task_return_xml
                           RESULT iobj = lo_data_container
                           OPTIONS clear = 'all'.
    lo_data_container->get(  EXPORTING id_id = 'LIST'
                             IMPORTING ex_data = lt_list ).
    lo_data_container->get(  EXPORTING id_id = 'LOG'
                             IMPORTING ex_data = lt_messages ).

    INSERT LINES OF lt_list INTO TABLE lt_list_all.
    INSERT LINES OF lt_messages INTO TABLE lt_messages_all.
  ENDLOOP.

  ASSIGN go_data->mr_list->* TO FIELD-SYMBOL(<lt_list>).
  go_data->mo_msglist->add_from_instance( go_task_handler->mo_msglist ).

  IF lt_list_all IS INITIAL.
    IF go_task_handler->mo_msglist->has_messages_of_msgty( 'E' ) = abap_true.
      go_task_handler->mo_msglist->get_last_message( EXPORTING id_msgty   = 'E'
                                                     IMPORTING es_message = DATA(ls_message)
                                                     EXCEPTIONS OTHERS    = 999 ).
      IF ls_message IS NOT INITIAL.
        MESSAGE ID      ls_message-msgid
                TYPE    ls_message-msgty
                NUMBER  ls_message-msgno
                WITH    ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.
      ENDIF.
      RETURN.
    ENDIF.
  ENDIF.

  <lt_list> = lt_list_all.
  go_data->mo_msglist->add_from_bapi( EXPORTING it_bapiret = lt_messages_all ).


ENDFORM.
```  

7. Create new class inheriting from ```CL_RECA_TASK_PROCESS``` and in there implement methods ```GET_NEXT_PACKAGE``` and ```RUN``` as follows. Remember to replace:
* ```<your_reportid>``` with the id of your REIS report as in table ```TIVISREP```.
* ```<your_reis_class>``` with your REIS DATA class.
* ```<your_list_table_type>``` with the dictionary based table type that will contain the resulting list to be shown.
* ```<your_method_to_filter_by_group_of_ids``` with the method that will allow you to filter the first DB selection to a group of different values 
(i.e, group of posting documents). If your report works with standard RE-FX objects maybe you could replace it by executing ```ADD_BUSOBJECT```

```abap
  CLASS-METHODS object_to_taskid_and_objectid
      IMPORTING
        id_object  TYPE recataskobjid
      EXPORTING
        ed_taskid   TYPE guid_22
        es_objectid TYPE zre_cl_reis_opex=>mtyp_s_filter_by_docfi .
        
  METHOD object_to_taskid_and_objectid.
    ed_taskid   = id_object(22).
    es_objectid = id_object+22.
  ENDMETHOD.

  METHOD if_reca_task~get_next_package.

    DATA ld_taskid     TYPE mtyp_d_taskid.
    DATA ld_taskid_cmp TYPE mtyp_d_taskid.

    IF lines( mt_object ) = 0.
      RAISE no_more_data.
    ENDIF.

    LOOP AT mt_object ASSIGNING FIELD-SYMBOL(<ls_object>).
      DATA(ld_tabix) = sy-tabix.

      object_to_taskid_and_objectid( EXPORTING id_object   = <ls_object>
                                     IMPORTING ed_taskid   = ld_taskid ).

      IF ( ld_taskid_cmp  IS INITIAL ).
        ld_taskid_cmp = ld_taskid.
      ELSEIF ( ld_taskid_cmp <> ld_taskid ) AND
             ( ld_taskid IS NOT INITIAL ).
        EXIT.
      ENDIF.
      INSERT <ls_object> INTO TABLE et_object.
      DELETE mt_object INDEX ld_tabix.
    ENDLOOP.

  ENDMETHOD.
    
  
  METHOD if_reca_task~run.


    DATA lt_filter_by_objectid TYPE zre_cl_reis_opex=>mtyp_t_filter_by_docfi.

    "Resulting list should be dictionary based typed, if not serialization
    "errors will occur
    FIELD-SYMBOLS <lt_list> TYPE <your_list_table_type>.

    "Execute REIS list 
    cl_reis_ca_services=>create_rfw_instance(
      EXPORTING
        id_reportid = <your_reportid>
      IMPORTING
        eo_data     = DATA(lo_data)
        eo_view     = DATA(lo_view)
      EXCEPTIONS
        OTHERS      = 999 ).

    lo_data->set_additional_param( mo_param ).
    LOOP AT mt_object ASSIGNING FIELD-SYMBOL(<ls_object>).
      object_to_taskid_and_objectid( EXPORTING id_object   = <ls_object>
                                     IMPORTING es_objectid = DATA(ls_filter_by_objectid) ).
      INSERT ls_filter_by_objectid INTO TABLE lt_filter_by_objectid.
    ENDLOOP.
    
    " Use this line if your report do not work with ADD_BUSOBJECT method
    CAST <your_reis_class>( lo_data )-><your_method_to_filter_by_group_of_ids>( lt_filter_by_objectid ).
    
    " Use this code if your report works with ADD_BUSOBJECT
    LOOP at lt_filter_by_objectid into ls_filter_by_objectid.
      CF_RECA_BUS_OBJECT=>FIND_BY_OBJNR( EXPORTING  id_objnr = ls_filter_by_objectid 
                                                    id_activity = reca1_activity-display 
                                         RECEIVING  ro_instance = data(lo_busobject)
                                         EXCEPTIONS others      = 999 ).
      if sy-subrc is initial.                                         
        lo_data->add_busobject( ls_filter_by_objectid ).
      endif.
    ENDLOOP.
    
    " Execute list
    lo_data->complete_data(  ).
    " Ensure BADI is executed
    lo_data->exit_change_output_data( ).
    
    " Retrieve list
    ASSIGN lo_data->mr_list->* TO <lt_list>.
    lo_data->mo_msglist->get_list_as_bapiret( IMPORTING et_list = DATA(lt_messages) ).
    DATA(lo_container) = NEW cl_reca_data_container( ).
    
    " Serialize both, list and error log
    lo_container->add( EXPORTING  id_id   = 'LIST'
                                  ix_data = <lt_list>
                       EXCEPTIONS OTHERS  = 999 ).
    lo_container->add( EXPORTING  id_id   = 'LOG'
                                  ix_data = lt_messages
                       exceptions others  = 999 ).
    CALL TRANSFORMATION id SOURCE iobj = lo_container
                           RESULT XML ed_result_xml.

    lo_container->clear( ).


    DATA(ls_statistics) = lo_data->mo_msglist->get_statistics( ).
    IF ( ls_statistics-msg_cnt_a IS NOT INITIAL ) OR
       ( ls_statistics-msg_cnt_e IS NOT INITIAL ).
      es_summary-haserror = abap_true.
    ENDIF.
    IF ls_statistics-msg_cnt_w IS NOT INITIAL.
      es_summary-haswarning = abap_true.
    ENDIF.
    es_summary-msg_count = ls_statistics-msg_cnt_al.
    es_summary-loghandle = lo_data->mo_msglist->get_handle( ).


    IF lo_data->mo_msglist->has_messages_of_msgty(
            id_msgty = lo_data->md_msgty_filter ) = abap_true.

      IF ( lo_data->md_msgty_filter = 'I' ).
        DATA(lf_i_messages) = abap_true.
      ENDIF.

      lo_data->mo_msglist->get_statistics_as_text(
          EXPORTING
            if_output_e_messages   = abap_true
            if_output_w_messages   = abap_true
            if_output_i_messages   = lf_i_messages
            if_output_highest_only = abap_false
          IMPORTING
            ed_text                = DATA(ld_err_stat) ).

      MESSAGE s000(recabc) WITH 'Existen mensajes'(001) '-' ld_err_stat ''.
      es_summary-msgty = sy-msgty.
      es_summary-msgid = sy-msgid.
      es_summary-msgno = sy-msgno.
      es_summary-msgv1 = sy-msgv1.
      es_summary-msgv2 = sy-msgv2.
      es_summary-msgv3 = sy-msgv3.
      es_summary-msgv4 = sy-msgv4.

    ENDIF.
  ENDMETHOD.
```  
8. Finally add entry via SM30 ```V_TIVCAIMPL```

| ImplfName | ImplSubType | ImplClName | 
|---|---|---|
| IF_RECA_TASK | <your_reportid> | <your_task_class>
