INTERFACE zif_excel_ixml_node_iterator
  PUBLIC.

  METHODS get_next
    RETURNING
      VALUE(rval) TYPE REF TO zif_excel_ixml_node.

ENDINTERFACE.
