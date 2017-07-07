*&---------------------------------------------------------------------*
*& Report ZBP_CHANGE_CATEGORY_BP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbp_change_category_bp.

DATA ls_return  TYPE bapiret2.
DATA gv_years   TYPE num02.
DATA gv_url_api TYPE string.
DATA lit_range  TYPE zttbc_range.
DATA rng_katr2  TYPE RANGE OF katr2.
DATA rng_katr3  TYPE RANGE OF katr3.

START-OF-SELECTION.

**********************************************************************
*.... Limite de Años para cambio de categoria
  CALL METHOD zcl_bc_udc_utilities=>get_constant
    EXPORTING
      im_progname    = sy-repid
      im_id_constant = 'LIMITE_AÑOS_SOCIO'
    IMPORTING
      ex_value       = DATA(lv_years).
  CONDENSE lv_years.
  MOVE lv_years TO gv_years.
**********************************************************************
*.... URL_API
  CALL METHOD zcl_bc_udc_utilities=>get_constant
    EXPORTING
      im_progname    = sy-repid
      im_id_constant = 'URL_API'
    IMPORTING
      ex_value       = DATA(lv_url_api).
  CONDENSE lv_url_api.
  MOVE lv_url_api TO gv_url_api.
**********************************************************************
  FREE lit_range[].
  zcl_bc_udc_utilities=>get_range(
    EXPORTING
      im_progname = sy-repid
      im_range    = 'ID_CONSANGUINIDAD'
    IMPORTING
      ex_range    = lit_range
  ).
  rng_katr2[] = lit_range[].
**********************************************************************
  FREE lit_range[].
  zcl_bc_udc_utilities=>get_range(
    EXPORTING
      im_progname = sy-repid
      im_range    = 'ID_ESTADO'
    IMPORTING
      ex_range    = lit_range
  ).
  rng_katr3[] = lit_range[].

**********************************************************************

  DATA(lo_object) = NEW zcl_bp_partner( i_years    = gv_years
                                        i_url_api  = gv_url_api
                                        i_rng_katr2 = rng_katr2
                                        i_rng_katr3 = rng_katr3 ).

  BREAK con_abap3.
  lo_object->sync_object( IMPORTING es_return = ls_return ).

**********************************************************************
