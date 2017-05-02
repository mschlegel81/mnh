UNIT searchModel;
INTERFACE
TYPE 
  T_searchReplaceModel=object
    private
      FindDialog    :TFindDialog;
      ReplaceDialog :TReplaceDialog;
      assignedEditor:TSynEdit;
    public
  end;   

VAR searchReplaceModel:T_searchReplaceModel;
IMPLEMENTATION

end.