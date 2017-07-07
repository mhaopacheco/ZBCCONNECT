class ZCL_BC_CONNECT_HCP definition
  public
  create public .

public section.

  methods EXECUTE_SERVICE
    importing
      !ID_PATH type STRING
      !ID_METHOD type STRING
      !ID_INPUT type XSTRING optional
    exporting
      !ED_RESULT type STRING .
  methods TRANSFORM_DATA_TO_JSON
    importing
      !IT_DATA type TABLE
    exporting
      !ED_JSON type STRING .
  methods TRANSFORM_JSON_TO_DATA
    importing
      !ID_JSON type STRING
    exporting
      !ES_RESULT type ANY
    raising
      ZCX_JSON_DOCUMENT .
  methods CONSTRUCTOR
    importing
      !USERNAME type STRING optional
      !PASSWORD type STRING optional
      !DESTINATION_URL type STRING .
protected section.
private section.

  data CLIENT type SYMANDT .
  data USERNAME type STRING .
  data PASSWORD type STRING .
  data LANGUAGE type SYLANGU .
  data DESTINATION type RFCDEST .
ENDCLASS.



CLASS ZCL_BC_CONNECT_HCP IMPLEMENTATION.


METHOD constructor.

  me->username  = username.
  me->password  = password.
  me->language  = sy-langu.
  me->client    = sy-mandt.
  me->destination = destination_url .

  IF username IS INITIAL AND password IS INITIAL .
    me->username = 'S0016387992' .
    me->password = 'Root@123' .
  ENDIF.

ENDMETHOD.


METHOD execute_service.

  CONCATENATE destination id_path INTO DATA(ld_service).

  cl_http_client=>create_by_destination(
    EXPORTING
      destination              = me->destination   " Logical destination (specified in function call)
    IMPORTING
      client                   = DATA(lo_http_client)    " HTTP Client Abstraction
*    EXCEPTIONS
*      argument_not_found       = 1
*      destination_not_found    = 2
*      destination_no_authority = 3
*      plugin_not_active        = 4
*      internal_error           = 5
*      others                   = 6
  ).
*

  cl_http_utility=>set_request_uri( request = lo_http_client->request
                               uri  = id_path ).

*  cl_http_client=>create_by_url(
*       EXPORTING
*         url                = ld_service
*       IMPORTING
*         client             = DATA(lo_http_client)
*       EXCEPTIONS
*         argument_not_found = 1
*         plugin_not_active  = 2
*         internal_error     = 3
*         OTHERS             = 4 ).

  CALL METHOD lo_http_client->authenticate(
    EXPORTING
      client   = me->client
      username = me->username
      password = me->password
      language = me->language ).

  "****Set the Request type to GET
  lo_http_client->request->set_header_field( name  = '~request_method'
                           value = id_method ).                 "#EC *

  IF id_input IS NOT INITIAL.
    lo_http_client->request->set_content_type( 'application/json' ).
    lo_http_client->request->set_data( id_input ).
  ENDIF.

  lo_http_client->send(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2 ).

  lo_http_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3 ).

  IF sy-subrc <> 0.
    CALL METHOD lo_http_client->get_last_error
      IMPORTING
        code    = DATA(subrc)
        message = DATA(errortext).

*    WRITE: / 'communication_error( receive )',
*           / 'code: ', subrc, 'message: ', errortext.
    EXIT.
  ELSE.
****Get the response content in Character format
    DATA(lv_result) = lo_http_client->response->get_cdata( ).
    ed_result = lv_result.
  ENDIF.

  "Prepare XML structure
  lo_http_client->close( ).


*  WRITE : 'Test Report to cosume OData service'.
*  WRITE: /, lv_result.

ENDMETHOD.


METHOD transform_data_to_json.

  DATA(json_doc) = zcl_json_document=>create_with_data( data = it_data[]"sbook_t
                                                suppress_itab = abap_true ).

  DATA(json) = json_doc->get_json( ).

  ed_json = json.

ENDMETHOD.


METHOD transform_json_to_data.

  DATA(json_doc) = zcl_json_document=>create_with_json( id_json ).
  json_doc->get_data(
    IMPORTING
      data              = es_result"[] "sbook_t[]
  ).

ENDMETHOD.
ENDCLASS.
