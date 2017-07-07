class ZCL_BP_PARTNER definition
  public
  inheriting from ZCL_EXAMPLE_CONSUME_API
  final
  create public .

public section.

  data MD_PARTNER type PARTNER .
  data MD_YEARS_PARTNER type NUM02 .
  data MD_URL_API type STRING .
  data:
    rng_katr2 TYPE RANGE OF kna1-katr2 .
  data:
    rng_katr3 TYPE RANGE OF kna1-katr3 .

  methods CONSTRUCTOR
    importing
      value(I_YEARS) type NUM02
      value(I_URL_API) type STRING
      !I_RNG_KATR2 like RNG_KATR2
      !I_RNG_KATR3 like RNG_KATR3 .
protected section.

  methods CREATE_EJECUTION
    redefinition .
  methods CREATE_JSON
    redefinition .
  methods GET_DATA
    redefinition .
  methods CREATE_MSG_INF
    redefinition .
PRIVATE SECTION.

  TYPES:
    BEGIN OF lty_json ,
      numero_socio TYPE char7,
    END OF lty_json .
  TYPES:
    BEGIN OF lty_message ,
      operation TYPE char45,
      message   TYPE string,
    END OF lty_message.

  TYPES:
    BEGIN OF lts_result.
*         INCLUDE TYPE ty_data.
  TYPES data    TYPE STANDARD TABLE OF lty_json WITH DEFAULT KEY.
  TYPES state   TYPE i.
*  TYPES message TYPE string.
  TYPES message TYPE STANDARD TABLE OF lty_message WITH DEFAULT KEY.
  TYPES: END OF lts_result .
  TYPES:
    BEGIN OF gty_partnes,
      partner TYPE bu_partner,
    END OF gty_partnes .

  DATA gv_years_partner TYPE num02 .
  DATA:
    gdt_partners  TYPE TABLE OF gty_partnes .

  METHODS call_api
    IMPORTING
      VALUE(i_path) TYPE string
    EXPORTING
      VALUE(i_data) TYPE lts_result .
  METHODS update_bp
    IMPORTING
      VALUE(ip_partner)     TYPE bu_partner
      VALUE(ip_new_derecho) TYPE char7
    EXPORTING
      VALUE(ep_return)      TYPE bapiret2 .
ENDCLASS.



CLASS ZCL_BP_PARTNER IMPLEMENTATION.


  METHOD call_api.

    "Crea Conexíón
    DATA(lo_rest) = NEW zcl_bc_connect_hcp( destination_url = 'HCP_US2_CCB').

    lo_rest->execute_service(
  EXPORTING
    id_path   = i_path
    id_method = 'GET'
    id_input  = md_payload
  IMPORTING
    ed_result = DATA(ld_json)
).

*    IF ld_json IS INITIAL.
*      es_return-type = 'W'.
*      es_return-message = 'Respuesta JSON Vacia'.
*      RETURN.
*    ENDIF.

    CHECK ld_json IS NOT INITIAL.

    lo_rest->transform_json_to_data(
      EXPORTING
        id_json           = ld_json
      IMPORTING
        es_result         = i_data
    ).

  ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  md_years_partner = i_years .
  md_url_api       = i_url_api.
  rng_katr2        = i_rng_katr2.
  rng_katr3        = i_rng_katr3.

ENDMETHOD.


METHOD create_ejecution.

  DATA: lv_partner TYPE bu_partner.
  DATA: lv_path    TYPE string.
  DATA: ls_data    TYPE lts_result.

  CHECK gdt_partners[] IS NOT INITIAL.

  LOOP AT gdt_partners ASSIGNING FIELD-SYMBOL(<partners>).

    CLEAR: lv_partner, es_return.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = <partners>-partner
      IMPORTING
        output = lv_partner.

    CLEAR lv_path.
    CONCATENATE md_url_api lv_partner INTO lv_path.

    me->call_api( EXPORTING i_path = lv_path
                  IMPORTING i_data = ls_data ).

    CASE ls_data-state.
      WHEN '1'.
        READ TABLE ls_data-data ASSIGNING FIELD-SYMBOL(<data>) INDEX 1.
        IF <data> IS ASSIGNED.
**********************************************************************
*.... Modify Bussines Partner
          me->update_bp( EXPORTING ip_partner     = <partners>-partner
                                   ip_new_derecho = <data>-numero_socio
                         IMPORTING ep_return      = es_return ).
        ENDIF.
      WHEN '2' OR '3'.
        es_return-type       = 'E'.
        READ TABLE ls_data-message ASSIGNING FIELD-SYMBOL(<message>) INDEX 1.
        CONCATENATE <message>-message 'tercero No' <partners>-partner
               INTO es_return-message SEPARATED BY space .
    ENDCASE.
**********************************************************************
*.... Log Messages
    me->create_msg_inf( EXPORTING is_return = es_return ) .
  ENDLOOP.

ENDMETHOD.


METHOD create_json.



ENDMETHOD.


  METHOD create_msg_inf.

    " *******************************************************************
    "1 Creo Mensaje
    DATA: ld_object_key     TYPE swo_typeid .

*    Todo Crear Key
    MOVE is_return-message_v1 TO ld_object_key .

    zcl_pi_message=>get_instance(
      EXPORTING
        id_interface  = 'SYNC_CHANGE_CATEGORY_BP'     " Name of Interface
*      id_meindex   =     " Índice de mbomensajes (número actual)
        id_commit     = abap_false
        id_object_key = ld_object_key
      RECEIVING
        eo_message   =  DATA(lo_message)   " Definition Message Interface
    ).

    CHECK lo_message IS NOT INITIAL.
    " *******************************************************************
    "Calcula Estado in ZTPI_MESSAGE
    CASE is_return-type.
      WHEN 'S'.
        lo_message->set_status( i_status = '1' ).
        lo_message->set_message( i_message = is_return-message ).
      WHEN 'E'.
        lo_message->set_status( i_status = '4' ).
        lo_message->set_message( i_message = is_return-message ).
      WHEN OTHERS.
        lo_message->set_status( i_status = '3' ).
        lo_message->set_message( i_message = is_return-message ).
    ENDCASE.
    " *******************************************************************
    "3. Lleno tabla de mensaje
    DATA: ls_ztpi_syn_obj    TYPE ztpi_syn_obj.

    ls_ztpi_syn_obj-meindex     = lo_message->get_meindex( ).
    ls_ztpi_syn_obj-object_key  = ld_object_key .
    ls_ztpi_syn_obj-json        = md_json .
    INSERT ztpi_syn_obj FROM ls_ztpi_syn_obj.
    IF sy-subrc <> 0 .
      es_return-type = 'E' .
      es_return-message = 'Error al Insertar' .
    ENDIF.

    COMMIT WORK AND WAIT .











  ENDMETHOD.


  METHOD get_data.

**********************************************************************

    SELECT a~kunnr, a~katr1, a~katr2, b~birthdt
         FROM kna1 AS a INNER JOIN but000 AS b ON a~kunnr = b~partner
         INTO TABLE @DATA(ldt_kna1)
         WHERE a~katr2 IN @rng_katr2
           AND a~katr3 IN @rng_katr3.

    IF sy-subrc EQ 0.
      LOOP AT ldt_kna1 ASSIGNING FIELD-SYMBOL(<kna1>).

        CHECK <kna1>-birthdt IS NOT INITIAL.
        CLEAR gv_years_partner.
        CALL FUNCTION 'COMPUTE_YEARS_BETWEEN_DATES'
          EXPORTING
            first_date                  = <kna1>-birthdt
            second_date                 = sy-datum
          IMPORTING
            years_between_dates         = gv_years_partner
          EXCEPTIONS
            sequence_of_dates_not_valid = 1
            OTHERS                      = 2.
        IF sy-subrc EQ 0.
          CONDENSE gv_years_partner.
        ENDIF.

        IF gv_years_partner GE md_years_partner.
          APPEND INITIAL LINE TO gdt_partners ASSIGNING FIELD-SYMBOL(<partners>).
          MOVE <kna1>-kunnr TO <partners>-partner .
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD update_bp.

    DATA: lt_updatebp TYPE zttfi_updatebp,
          ls_updatebp TYPE zstfi_updatebp.

    SELECT SINGLE partner, bu_sort1, bu_sort2, katr1, katr2, katr3
                  FROM but000 AS b INNER JOIN kna1 AS k ON b~partner = k~kunnr
                  INTO @DATA(ls_but000)
                  WHERE partner EQ @ip_partner.   " Parametro Import

    CHECK sy-subrc = 0.
    ls_updatebp-bu_partner  = ls_but000-partner.
    ls_updatebp-id_derecho  = ip_new_derecho.     " Parametro Import
    ls_updatebp-tit_derecho = ls_but000-bu_sort2.
    ls_updatebp-katr2       = ls_but000-katr2.

    CASE ls_but000-katr2 .
      WHEN '04'.
        MOVE '03' TO ls_updatebp-katr1.
      WHEN '05'.
        MOVE '04' TO ls_updatebp-katr1.
    ENDCASE.
    APPEND ls_updatebp  TO lt_updatebp.

*..... Modificación Intelocutor
    DATA(lo_bapibp) = NEW zcl_update_bp( it_updatebp = lt_updatebp ).
    lo_bapibp->update_bp( IMPORTING it_updatebp = lt_updatebp ).

    LOOP AT lt_updatebp TRANSPORTING NO FIELDS WHERE message IS INITIAL.
      ep_return-type    = 'S'.
      CONCATENATE 'Se realizó cambio de categoria para el socio' ip_partner
             INTO ep_return-message SEPARATED BY space.
    ENDLOOP.
    IF sy-subrc NE 0.
      LOOP AT lt_updatebp ASSIGNING FIELD-SYMBOL(<update>) WHERE message IS NOT INITIAL.
        ep_return-type    = 'E'.
        CONCATENATE <update>-message 'Tercero No' ip_partner
               INTO ep_return-message SEPARATED BY space.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
