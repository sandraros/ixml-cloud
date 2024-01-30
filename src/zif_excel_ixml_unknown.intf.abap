INTERFACE zif_excel_ixml_unknown
  PUBLIC.

  METHODS query_interface
    IMPORTING
      !iid TYPE i
    RETURNING
      VALUE(rval) TYPE REF TO zif_excel_ixml_unknown.

ENDINTERFACE.
