INTERFACE zif_excel_ixml_node_iterator
  PUBLIC .

  methods GET_NEXT
    returning
      value(RVAL) type ref to zif_excel_IXML_NODE .

ENDINTERFACE.
