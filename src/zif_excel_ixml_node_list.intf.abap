INTERFACE zif_excel_ixml_node_list
  PUBLIC .

  methods CREATE_ITERATOR
    returning
      value(RVAL) type ref to zif_excel_IXML_NODE_ITERATOR .
ENDINTERFACE.
