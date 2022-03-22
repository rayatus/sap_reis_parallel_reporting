CLASS zcl_reca_reis_demo_parallel DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM cl_reis_data_cn.

  PUBLIC SECTION.
    METHODS set_additional_param REDEFINITION.
    INTERFACES zif_reca_reis_parallel_task.
    METHODS complete_data REDEFINITION.

  PROTECTED SECTION.
    DATA mt_contracts TYPE re_t_intreno.
  PRIVATE SECTION.
    TYPES: BEGIN OF mtyp_s_filter_by,
             bukrs   TYPE RANGE OF vicncn-bukrs,
             recnnr  TYPE RANGE OF vicncn-recnnr,
             intreno TYPE re_t_intreno,
           END   OF mtyp_s_filter_by.
    DATA   ms_filter_by TYPE mtyp_s_filter_by.
    METHODS get_contracts.
ENDCLASS.



CLASS ZCL_RECA_REIS_DEMO_PARALLEL IMPLEMENTATION.


  METHOD complete_data.

    get_contracts(  ).

    LOOP AT mt_contracts INTO DATA(ld_contract).
      cf_recn_contract=>find_by_intreno( EXPORTING id_activity   = reca1_activity-display
                                                   id_intreno    = ld_contract
                                                   if_auth_check = abap_false
                                         RECEIVING ro_instance   = DATA(lo_contract)
                                         EXCEPTIONS OTHERS       = 999 ).
      IF sy-subrc IS INITIAL.
        add_busobj( io_busobj = lo_contract ).
      ELSE.
        mo_msglist->add_symsg(  ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_contracts.

    IF ms_filter_by-intreno IS NOT INITIAL.

      mt_contracts = ms_filter_by-intreno.

    ELSE.
      SELECT DISTINCT intreno
      INTO TABLE mt_contracts
      FROM vicncn
      WHERE bukrs  IN ms_filter_by-bukrs
        AND recnnr IN ms_filter_by-recnnr.
    ENDIF.
  ENDMETHOD.


  METHOD set_additional_param.
    io_param->get( EXPORTING id_id   = 'BUKRS'
                   IMPORTING ex_data = ms_filter_by-bukrs[] ).
    io_param->get( EXPORTING id_id   = 'RECNNR'
                   IMPORTING ex_data = ms_filter_by-recnnr[] ).
  ENDMETHOD.


  METHOD zif_reca_reis_parallel_task~get_filter_by_objectid.
    get_contracts(  ).
    LOOP AT mt_contracts INTO DATA(ld_contract).
      INSERT CONV #( ld_contract ) INTO TABLE et_objectid.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_reca_reis_parallel_task~set_filter_by_objectid.
    LOOP AT it_objectid INTO DATA(ld_objectid).
      INSERT CONV #( ld_objectid ) INTO TABLE ms_filter_by-intreno.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
