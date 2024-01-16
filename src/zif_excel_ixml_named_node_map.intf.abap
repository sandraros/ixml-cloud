INTERFACE zif_excel_ixml_named_node_map
  PUBLIC .

  methods CREATE_ITERATOR
    returning
      value(RVAL) type ref to zIF_excel_IXML_NODE_ITERATOR .

ENDINTERFACE.
