INTERFACE zif_excel_ixml_parser
  PUBLIC .

  constants CO_NO_VALIDATION type I value 0. "#EC NOTEXT

  methods ADD_STRIP_SPACE_ELEMENT
    importing
      !NAME type STRING default '*'
      !URI type STRING default ''
    returning
      value(RVAL) type ABAP_BOOLEAN .

  methods PARSE
    returning
      value(RVAL) type I .

  methods SET_NORMALIZING
    importing
      !IS_NORMALIZING type BOOLEAN default 'X'
    returning
      value(RVAL) type BOOLEAN .

  methods SET_VALIDATING
    importing
      !MODE type I default '1'
    returning
      value(RVAL) type BOOLEAN .

ENDINTERFACE.
