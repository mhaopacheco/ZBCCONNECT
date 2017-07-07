class ZCL_EXAMPLE_CONSUME_API definition
  public
  create public .

public section.

  data MD_PAYLOAD type XSTRING .

  methods SYNC_OBJECT
    exporting
      !ES_RETURN type BAPIRET2 .
protected section.

  types:
    BEGIN OF TY_JSON ,
            data TYPE string ,
          END OF ty_json .
  types:
    tty_json TYPE STANDARD TABLE OF ty_json .
  types:
    BEGIN OF ts_result.
*         INCLUDE TYPE ty_data.
    TYPES data    TYPE STANDARD TABLE OF ty_json WITH DEFAULT KEY.
    TYPES state   TYPE i.
    TYPES message TYPE string.
  TYPES: END OF ts_result .

  data MD_JSON type STRING .

  methods CREATE_JSON
    exporting
      !ED_PAYLOAD type XSTRING .
  methods CREATE_MSG_INF
    importing
      !IS_RETURN type BAPIRET2
    exporting
      !ES_RETURN type BAPIRET2 .
  methods CREATE_EJECUTION
    exporting
      !ES_RETURN type BAPIRET2 .
  methods GET_DATA .
private section.
ENDCLASS.



CLASS ZCL_EXAMPLE_CONSUME_API IMPLEMENTATION.


METHOD create_ejecution.

  "Crea Conexíón
  DATA(lo_rest) = NEW zcl_bc_connect_hcp( destination_url = 'HCP_US2_CCB').

  "ToDo Concatenar bussiness partner
  "BEGINOF: MPACHECO: 09.05.2017 17:06:59
  "Obser: add id_partner
  DATA: ld_path     TYPE string.

  CONCATENATE 'rest/socio/cambio/hijodesocio/' 'ToDo' INTO ld_path .

  "ENDOF: MPACHECO: 09.05.2017 17:07:04

  lo_rest->execute_service(
    EXPORTING
      id_path   = ld_path
      id_method = 'GET'
      id_input  = md_payload
    IMPORTING
      ed_result = DATA(ld_json)
  ).

  IF ld_json IS INITIAL.
    es_return-type = 'W'.
    es_return-message = 'Respuesta JSON Vacia'.
    RETURN.
  ENDIF.

  DATA: ls_data     TYPE ts_result.

  lo_rest->transform_json_to_data(
    EXPORTING
      id_json           = ld_json
    IMPORTING
      es_result         = ls_data
  ).

  es_return-message = ls_data-message .
  CASE ls_data-state.
    WHEN '1'.
      es_return-type = 'S'.
      es_return-message = 'Transacción Exitosa'.
    WHEN '2' OR '3'.
      es_return-type = 'E'.
    WHEN OTHERS.
  ENDCASE.

ENDMETHOD.


METHOD create_json.
ENDMETHOD.


METHOD create_msg_inf.

  " *******************************************************************
  "1 Creo Mensaje
  DATA: ld_object_key     TYPE swo_typeid .

  "Todo Crear Key

*  CONCATENATE md_matnr md_werks md_stlan INTO ld_object_key .

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



ENDMETHOD.


METHOD sync_object.

  me->get_data( ) .
  me->create_ejecution( IMPORTING es_return = es_return ) .

ENDMETHOD.
ENDCLASS.
