CLASS zcl_reca_reis_task_process DEFINITION
  PUBLIC
  ABSTRACT
  INHERITING FROM cl_reca_task_process
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF mc_container,
                 resulting_list TYPE recastring VALUE 'LIST',
               END OF mc_container.

    METHODS taskid_and_objectid_to_object
      IMPORTING
                id_taskid        TYPE zif_reca_reis_parallel_task=>mtyp_d_taskid
                id_objectid      TYPE zif_reca_reis_parallel_task=>mtyp_d_objectid
      RETURNING VALUE(rd_object) TYPE recataskobjid.

    METHODS object_to_taskid_and_objectid
      IMPORTING
        id_object   TYPE recataskobjid
      EXPORTING
        ed_taskid   TYPE zif_reca_reis_parallel_task=>mtyp_d_taskid
        es_objectid TYPE zif_reca_reis_parallel_task=>mtyp_d_objectid .

    METHODS get_reportid
          ABSTRACT
      RETURNING
        VALUE(rd_reportid) TYPE reisrepid .

    CLASS-METHODS check_serialization_possible
      IMPORTING
        io_msglist   TYPE REF TO if_reca_message_list
        io_container TYPE REF TO cl_reca_data_container.

    METHODS if_reca_task~get_next_package
        REDEFINITION .
    METHODS if_reca_task~run
        REDEFINITION .
  PROTECTED SECTION.

  PRIVATE SECTION.



ENDCLASS.



CLASS ZCL_RECA_REIS_TASK_PROCESS IMPLEMENTATION.


  METHOD check_serialization_possible.

    io_container->get_list( IMPORTING et_list = DATA(lt_container_data) ).
    LOOP AT lt_container_data INTO DATA(ls_container_data) WHERE type IS INITIAL.
      MESSAGE e000(recabc) WITH 'XML Serialization error : container data'(002)
                                ls_container_data-id
                                ' is not defined in Dictionary'(003)
                                space
                                INTO DATA(ld_dummy).
      io_msglist->add_symsg(  ).
    ENDLOOP.

  ENDMETHOD.


  METHOD if_reca_task~get_next_package.
    DATA ld_taskid     TYPE zif_reca_reis_parallel_task=>mtyp_d_taskid.
    DATA ld_taskid_cmp LIKE ld_taskid.

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

    DATA lt_object_id TYPE zif_reca_reis_parallel_task=>mtyp_t_objectid.
    DATA ld_string TYPE string.
    FIELD-SYMBOLS <lt_list> TYPE ANY TABLE.

    DATA(lo_msglist)  = cf_reca_message_list=>create(  ).
    DATA(ld_reportid) = get_reportid( ).

    "Ensure that it exists a REIS report with this ID
    cl_reisc_reportid=>get_detail( EXPORTING  id_reportid = ld_reportid
                                              id_require  = 'K'
                                   RECEIVING  rs_detail   = DATA(ls_reportid_detail)
                                   EXCEPTIONS OTHERS      = 999 ).
    IF sy-subrc IS NOT INITIAL.
      "Report id does not exist.
      lo_msglist->add_symsg(  ).
    ENDIF.

    "Ensure that custo is correctly filled
    cl_recac_interface_impl=>check_key( EXPORTING  id_implifname  = 'IF_RECA_TASK'
                                                   id_implsubtype = CONV #( get_reportid(  ) )
                                        EXCEPTIONS OTHERS         = 999 ).
    IF sy-subrc IS NOT INITIAL.
      lo_msglist->add_symsg(  ).
    ENDIF.

    IF lo_msglist->has_messages_of_msgty( 'E' ) = abap_false.

      "Execute REIS list
      cl_reis_ca_services=>create_rfw_instance(
           EXPORTING
             id_reportid = ld_reportid
           IMPORTING
             eo_data     = DATA(lo_data)
             eo_view     = DATA(lo_view)
           EXCEPTIONS
             OTHERS      = 999 ).

      lo_data->set_additional_param( mo_param ).
      LOOP AT mt_object ASSIGNING FIELD-SYMBOL(<ls_object>).
        object_to_taskid_and_objectid( EXPORTING id_object   = <ls_object>
                                       IMPORTING es_objectid = DATA(ls_object_id) ).
        INSERT ls_object_id INTO TABLE lt_object_id.
      ENDLOOP.

      TRY.
          " Just execute the report for those objects in this parallel task
          CAST zif_reca_reis_parallel_task( lo_data )->set_filter_by_objectid( lt_object_id ).
          " Execute list
          lo_data->complete_data(  ).
          " Ensure BADI is executed
          lo_data->exit_change_output_data( ).
          " Retrieve list
          ASSIGN lo_data->mr_list->* TO <lt_list>.
          lo_msglist->add_from_instance( lo_data->mo_msglist ).
          " Serialize resulting list
          DATA(lo_container) = NEW cl_reca_data_container( ).

          LOOP AT <lt_list> ASSIGNING FIELD-SYMBOL(<ls_list>).
            cl_abap_container_utilities=>fill_container_c( EXPORTING im_value     = <ls_list>
                                                           IMPORTING ex_container = ld_string ).
            lo_container->add( EXPORTING  id_id   = zcl_reca_reis_task_process=>mc_container-resulting_list && sy-tabix
                                          ix_data = ld_string
                               EXCEPTIONS OTHERS  = 999 ).
          ENDLOOP.

          " Resulting list should be dictionary based typed, if not serialization error will occur
          DATA(lo_msglist_serialization) = cf_reca_message_list=>create(  ).
          check_serialization_possible( io_msglist   = lo_msglist_serialization io_container = lo_container ).

          IF lo_msglist_serialization->has_messages_of_msgty( 'E' ) = abap_false.
            CALL TRANSFORMATION id SOURCE iobj = lo_container
                                   RESULT XML ed_result_xml.
          ELSE.
            lo_msglist->add_from_instance( io_msglist = lo_msglist_serialization if_add_as_subnode = abap_true ).
          ENDIF.
          lo_msglist_serialization->free(  ).
          lo_container->clear( ).

        CATCH cx_sy_move_cast_error INTO DATA(lx_move_cast_error).
          " Reis report does not implements required interface
          lo_msglist->add_from_exception( lx_move_cast_error ).
      ENDTRY.

      DATA(ls_statistics) = lo_msglist->get_statistics( ).
      IF ( ls_statistics-msg_cnt_a IS NOT INITIAL ) OR
         ( ls_statistics-msg_cnt_e IS NOT INITIAL ).
        es_summary-haserror = abap_true.
      ENDIF.
      IF ls_statistics-msg_cnt_w IS NOT INITIAL.
        es_summary-haswarning = abap_true.
      ENDIF.

      es_summary-msg_count = ls_statistics-msg_cnt_al.
      IF lo_msglist->is_empty(  ) = abap_false.
        es_summary-loghandle = lo_msglist->get_handle( ).
        lo_msglist->store( EXPORTING if_in_update_task = abap_false EXCEPTIONS OTHERS = 999 ).
        es_summary-loghandledelete = abap_true.
      ENDIF.

    ENDIF.

    IF lo_msglist->has_messages_of_msgty( id_msgty = lo_data->md_msgty_filter ) = abap_true.

      IF ( lo_data->md_msgty_filter = 'I' ).
        DATA(lf_i_messages) = abap_true.
      ENDIF.

      lo_msglist->get_statistics_as_text(
          EXPORTING
            if_output_e_messages   = abap_true
            if_output_w_messages   = abap_true
            if_output_i_messages   = lf_i_messages
            if_output_highest_only = abap_false
          IMPORTING
            ed_text                = DATA(ld_err_stat) ).

      MESSAGE s000(recabc) WITH 'There are messages'(001) '-' ld_err_stat ''.
      es_summary-msgty = sy-msgty.
      es_summary-msgid = sy-msgid.
      es_summary-msgno = sy-msgno.
      es_summary-msgv1 = sy-msgv1.
      es_summary-msgv2 = sy-msgv2.
      es_summary-msgv3 = sy-msgv3.
      es_summary-msgv4 = sy-msgv4.

    ENDIF.

    lo_msglist->free(  ).

  ENDMETHOD.


  METHOD object_to_taskid_and_objectid.
    ed_taskid   = id_object(22).
    es_objectid = id_object+22.
  ENDMETHOD.


  METHOD taskid_and_objectid_to_object.
    CONCATENATE id_taskid id_objectid INTO rd_object.
  ENDMETHOD.
ENDCLASS.
