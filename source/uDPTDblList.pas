unit uDPTDblList;


interface

uses
  StdCtrls,
  Classes,
  Controls,
  Buttons;

const
  cButtonSize=24;
 // cComponentRegisterTab='NVBComponents';
  cComponentVersion='1.11';

type
  TNVBDblListOptions= set of (AllowAdd,  // allow to add a item to the list.
                              AllowDoubles, // allow to add the same item several times to the list
                              AllowRemove); // allow to remove an item from the list.

  TNVBOperationType=(nvbop_add,
                     nvbop_remove,
                     nvbop_move);

  TOnCheckItemEvent=function(Sender:TObject;const _ItemText:string;const _Operation:TNVBOperationType):boolean of object;

  { TNVBDblList }

  TNVBDblList = class(TGroupBox)
    private
        FSourceList : TListBox;
        FTargetList : TListBox;
        FAddAllItems : TBitBtn;
        FAddItem     : TBitBtn;
        FRemoveAllItems : TBitBtn;
        FRemoveItem  : TBitBtn;
        FOriginalSrcMouseDown:TMouseEvent; // the component users mouse down event of the source list
        FOriginalSrcDblClick:TNotifyEvent;
        FOriginalSrcEnter:TNotifyEvent;
        FOriginalSrcDragOver:TDragOverEvent;
        FOriginalSrcDragDrop:TDragDropEvent;
        FOriginalTrgMouseDown:TMouseEvent; // the component users mouse down event of the target list
        FOriginalTrgDblClick:TNotifyEvent;
        FOriginalTrgEnter:TNotifyEvent;
        FOriginalTrgDragOver:TDragOverEvent;
        FOriginalTrgDragDrop:TDragDropEvent;
        FOnChange:TNotifyEvent;            // if anything in the selection changes.
        FOnCheckItem:TOnCheckItemEvent;
        FVersion: string;
        FTargetOptions: TNVBDblListOptions;
        FSourceOptions: TNVBDblListOptions;
        procedure AutoInitialize;
        procedure AutoDestroy;
        function  GetSourceList : TListBox;
        procedure SetSourceList(Value : TListBox);
        function  GetTargetList : TListBox;
        procedure SetTargetList(Value : TListBox);
        procedure DoDragOver(Sender, Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
        procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer);
        procedure DoSrcListboxEnter(Sender:TObject);
        procedure DoTrgListboxEnter(Sender:TObject);
        function  DoCheckItem(const _ItemText:string;const _Operation:TNVBOperationType):boolean;
        procedure SetVersion(const Value: string);
        procedure SetSourceOptions(const Value: TNVBDblListOptions);
        procedure SetTargetOptions(const Value: TNVBDblListOptions);
    protected
        procedure Notification(AComponent : TComponent; Operation : TOperation); override;
        procedure Loaded; override;
        procedure AddItemBtnClick(Sender: TObject);
        procedure RemoveItemBtnClick(Sender: TObject);
        procedure AddAllItemsBtnClick(Sender: TObject);
        procedure RemoveAllItemsBtnClick(Sender: TObject);
        procedure SourceListBoxDoubleClickEvent(Sender: TObject);
        procedure SourceListBoxMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure TargetListBoxMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure TargetListBoxDoubleClickEvent(Sender: TObject);
        procedure MoveSelected(_from: TListBox;_to: TListBox);
        procedure SetItem(List: TListBox; Index: Integer);
        function  GetFirstSelection(List: TListBox): Integer;
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure SetButtons;  // verify the button states.
        property Version:string read FVersion write SetVersion;
    published
        property OnClick;
        property OnDblClick;
        property OnDragDrop;
        property OnEnter;
        property OnExit;
        property OnKeyDown;
        property OnKeyPress;
        property OnKeyUp;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        property OnCheckItem:TOnCheckItemEvent read FOnCheckItem write FOnCheckItem; // before an item gets added/removed to/from a list, the user can verify it in this event.
        property OnChange:TNotifyEvent read FOnChange write FOnChange;
        property SourceList : TListBox read GetSourceList write SetSourceList;
        property SourceOptions:TNVBDblListOptions read FSourceOptions write SetSourceOptions;
        property TargetList : TListBox read GetTargetList write SetTargetList;
        property TargetOptions:TNVBDblListOptions read FTargetOptions write SetTargetOptions;
  end;

//procedure Register;

implementation
//{$R cnvbdbllist.dcr}

uses
  Dialogs,
  Types;

{procedure Register;
begin
  RegisterComponents(cComponentRegisterTab, [TNVBDblList]);
end;}

//*************************************************************************
//Procedure: TNVBDblList.DragDrop
//Description: copy the item from the source list to the target list.
//Author: SH
//History: 23.09.2001 13:45:15
// 31.01.2004 - if the list is unsorted, then the drop position is the cursor position.
//*************************************************************************
procedure TNVBDblList.DoDragDrop(Sender, Source: TObject; X, Y: Integer);
var
_item:string;
_index:integer;
_previousIndex:integer;
_drop_position:integer;
begin
  _drop_position:=0;
  _index:=-1;
  if not (Source is TListBox) then exit;
  if not TListBox(Sender).Sorted then _drop_position:=TListBox(Sender).ItemAtPos(Point(X, Y), True);
  _previousIndex:=TListbox(Source).ItemIndex;
  _item:=TListbox(Source).items[_previousIndex];
  if Sender<>Source then begin // its an item that comes from the other list and has to be added.
    if (TListBox(Sender).Items.IndexOf(_item)=-1) or
      ((Sender=FSourceList) and (AllowDoubles in FSourceOptions)) or
      ((Sender=FTargetList) and (AllowDoubles in FTargetOptions))  then begin // check if the item is not already in the list.
        if ((Sender=FSourceList) and (AllowAdd in FSourceOptions)) or
          ((Sender=FTargetList) and (AllowAdd in FTargetOptions)) then begin
          if TListBox(Sender).Sorted then begin
            if DoCheckItem(_item,nvbop_add) then _index:=TListBox(Sender).Items.add(_item); // add the new item
          end
          else begin
            if DoCheckItem(_item,nvbop_add) then begin
              TListBox(Sender).Items.Insert(_drop_position,_item); // if the list is not sorted
              if _drop_position=-1 then _drop_position:=TListBox(Sender).Items.count-1;
              _index:=_drop_position;    // add the item at the position were the mouse pointer is.
            end;
          end;
          if _index<>-1 then TListBox(Sender).ItemIndex:=_index;
        end;
      end;
      if ((Source=FSourceList) and (AllowRemove in FSourceOptions)) or
         ((Source=FTargetList) and (AllowRemove in FTargetOptions)) then TListBox(Source).items.Delete(TListbox(Source).ItemIndex); // remove the item from the source list.
      SetButtons;
      if assigned(FOnChange) then FOnChange(self);
  end else begin  // its an item that comes from the same list, so we only have to move the position.
    if not TListBox(Sender).Sorted then begin // this make only sense if the list is not sorted.
      TListBox(Sender).items.Delete(_previousIndex);
      TListBox(Sender).Items.Insert(_drop_position,_item);
      if _drop_position=-1 then _drop_position:=TListBox(Sender).Items.count-1;
      TListBox(Sender).ItemIndex:=_drop_position;
      SetButtons;
      if assigned(FOnChange) then FOnChange(self);
    end;
  end;
end;

//*************************************************************************
//Procedure: TNVBDblList.DragOver
//Description: determine if the item can be accepted by the control.
//Author: SH
//History: 23.09.2001 13:36:15
//*************************************************************************
procedure TNVBDblList.DoDragOver(Sender, Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=(Source is TListBox);
end;

//*************************************************************************
//Procedure: TNVBDblList.SourceListBoxMouseDown
//Description: if the component user has assigned a mousedown event handler,
// then execute the users handler, otherwise start the drag and drop action.
//Author: SH
//History: 23.09.2001 13:23:43
//*************************************************************************
procedure TNVBDblList.SourceListBoxMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if assigned(FOriginalSrcMouseDown) then FOriginalSrcMouseDown(self,Button,Shift,x,y); // first execute the list components mouse down event
  if Button <> mbLeft then exit; { drag only if left button pressed }
  if TListBox(Sender).ItemAtPos(Point(X, Y), True) >= 0 then TListBox(Sender).BeginDrag(False); { is there an item here? }
end;

//*****************************************************
// Method:  TNVBDblList.SourceListBoxDoubleClickEvent
// Programmer: S.Herzog
// Description: if there is already an OnDblClick event handler assigend to the
// sourelist, then execute it. Otherwise execute this component code.
// Last changes: 21.11.01
//*****************************************************
procedure TNVBDblList.SourceListBoxDoubleClickEvent(Sender: TObject);
begin
  if assigned(FOriginalSrcDblClick) then FOriginalSrcDblClick(self);
  AddItemBtnClick(self);
end;

//*************************************************************************
//Procedure: TNVBDblList.TargetListBoxMouseDown
//Description: if the component user has assigned a mousedown event handler,
// then execute the users handler, otherwise start the drag and drop action.
//Author: SH
//History: 23.09.2001 13:23:43
//*************************************************************************
procedure TNVBDblList.TargetListBoxMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if assigned(FOriginalTrgMouseDown) then FOriginalTrgMouseDown(self,Button,Shift,x,y);
  if Button <> mbLeft then exit; { drag only if left button pressed }
  if TListBox(Sender).ItemAtPos(Point(X, Y), True) >= 0 then  TListBox(Sender).BeginDrag(False);  { if so, drag it }
end;

//*****************************************************
// Method:  TNVBDblList.TargetListBoxDoubleClickEvent
// Programmer: S.Herzog
// Description:
// Last changes: 21.11.01
//*****************************************************
procedure TNVBDblList.TargetListBoxDoubleClickEvent(Sender: TObject);
begin
  if assigned(FOriginalTrgDblClick) then FOriginalTrgDblClick(self);
  RemoveItemBtnClick(self);
end;

//*************************************************************************
//Procedure: TNVBDblList.AddItemBtnClick
//Description:
//Author: SH
//History: 22.09.2001 14:47:23
//*************************************************************************
procedure TNVBDblList.AddItemBtnClick(Sender: TObject);
var
  _index: Integer;
begin
  if (FSourceList=Nil) or (FTargetList=Nil) then exit;
  _index := GetFirstSelection(FSourceList);
  MoveSelected(FSourceList, FTargetList);
  SetItem(FSourceList, _index);
  if assigned(FOnChange) then FOnChange(self);
end;

//*************************************************************************
//Procedure: TNVBDblList.RemoveItemBtnClick
//Description:
//Author: SH
//History: 22.09.2001 14:47:26
//*************************************************************************
procedure TNVBDblList.RemoveItemBtnClick(Sender: TObject);
var
  _index: Integer;
begin
  if (FSourceList=Nil) or (FTargetList=Nil) then exit;
  FTargetList.Items.BeginUpdate;
  _index := GetFirstSelection(FTargetList);
  MoveSelected(FTargetList, FSourceList);
  SetItem(FTargetList, _index);
  FTargetList.Items.EndUpdate;
  if assigned(FOnChange) then FOnChange(self);
end;

//*************************************************************************
//Procedure: TNVBDblList.AddAllItemsBtnClick
//Description:
//Author: SH
//History: 22.09.2001 14:47:29
//*************************************************************************
procedure TNVBDblList.AddAllItemsBtnClick(Sender: TObject);
var
  i: Integer;
begin
  if (FSourceList=Nil) or (FTargetList=Nil) then exit;
  for i := 0 to FSourceList.Items.Count - 1 do begin
    if (FTargetList.Items.IndexOf(FSourceList.Items[I])=-1) or
       (AllowDoubles in FTargetOptions) then begin
      if DoCheckItem(FSourceList.Items[i],nvbop_add) then FTargetList.Items.AddObject(FSourceList.Items[I], FSourceList.Items.Objects[I]);
    end;
  end;
  if AllowRemove in FSourceOptions then begin
    FSourceList.Items.Clear;
    SetItem(FSourceList, 0);
  end;  
  if assigned(FOnChange) then FOnChange(self);
end;

//*************************************************************************
//Procedure: TNVBDblList.RemoveAllItemsBtnClick
//Description:
//Author: SH
//History: 22.09.2001 14:47:33
//*************************************************************************
procedure TNVBDblList.RemoveAllItemsBtnClick(Sender: TObject);
var
  i: Integer;
begin
  if (FSourceList=Nil) or (FTargetList=Nil) then exit;
  for i := 0 to FTargetList.Items.Count - 1 do begin
    if (AllowAdd in FSourceOptions) and
       ((FSourceList.Items.IndexOf(FTargetList.Items[i])=-1) or
       (AllowDoubles in FSourceOptions)) then begin
      if DoCheckItem(FTargetList.Items[i],nvbop_add) then FSourceList.Items.AddObject(FTargetList.Items[i], FTargetList.Items.Objects[I]);
    end;
  end;
  if AllowRemove in FTargetOptions then begin
    FTargetList.Items.Clear;
    SetItem(FTargetList, 0);
  end;
  if assigned(FOnChange) then FOnChange(self);
end;

//*************************************************************************
//Procedure: TNVBDblList.MoveSelected
//Description:
//Author: SH
//History: 22.09.2001 14:47:45
//*************************************************************************
procedure TNVBDblList.MoveSelected(_from: TListBox; _to: TListBox);
var
  i: Integer;
  _index:integer;
begin
  if _from=nil then exit;
  if _to=nil then exit;
  for i := _from.Items.Count - 1 downto 0 do begin
    if _from.Selected[I] then
    begin
      if _from=FSourceList then begin
        if (_to.Items.IndexOf(_from.Items[i])=-1) or
           (AllowDoubles in FTargetOptions) then begin
          if AllowAdd in FTargetOptions then begin
            if DoCheckItem(_from.Items[i],nvbop_add) then begin
              _index:=_to.Items.AddObject(_from.Items[i], _from.Items.Objects[i]);
              _to.ItemIndex:=_index;
            end;  
          end;
        end;
        if AllowRemove in FSourceOptions then _from.Items.Delete(i);
      end else begin
        if (_to.Items.IndexOf(_from.Items[i])=-1) or
           (AllowDoubles in FSourceOptions) then begin
          if AllowAdd in FSourceOptions then begin
            if DoCheckItem(_from.Items[i],nvbop_add) then begin
              _index:=_to.Items.AddObject(_from.Items[i], _from.Items.Objects[i]);
              _to.ItemIndex:=_index;
            end;  
          end;
        end;
        if AllowRemove in FTargetOptions then _from.Items.Delete(i);
      end;
    end;
  end;  
end;

//*************************************************************************
//Procedure: TNVBDblList.SetButtons
//Description: enable/disable the buttons according to the state of the lists.
//Author: SH
//History: 22.09.2001 14:47:49
// Bugfix 27.12.2002
//*************************************************************************
procedure TNVBDblList.SetButtons;
var
  _SrcEmpty,
  _DstEmpty: Boolean;
begin
  _SrcEmpty:=true;
  _DstEmpty:=true;
  if assigned(FSourceList) then _SrcEmpty := FSourceList.Items.Count = 0;
  if assigned(FTargetList) then _DstEmpty := FTargetList.Items.Count = 0;
  FAddItem.Enabled        := not _SrcEmpty;
  FAddAllItems.Enabled    := not _SrcEmpty;
  FRemoveItem.Enabled     := not _DstEmpty;
  FRemoveAllItems.Enabled := not _DstEmpty;
end;

//*************************************************************************
//Procedure: TNVBDblList.GetFirstSelection
//Description:
//Author: SH
//History: 22.09.2001 14:47:53
//*************************************************************************
function TNVBDblList.GetFirstSelection(List: TListBox): Integer;
begin
  Result:=0;
  if List=nil then exit;
  for Result := 0 to List.Items.Count - 1 do
    if List.Selected[Result] then Exit;
  Result := -1;
end;

//*************************************************************************
//Procedure: TNVBDblList.SetItem
//Description:
//Author: SH
//History: 22.09.2001 14:47:57
//*************************************************************************
procedure TNVBDblList.SetItem(List: TListBox; Index: Integer);
var
  _MaxIndex: Integer;
begin
  if List=nil then exit;
  List.SetFocus;
  _MaxIndex := List.Items.Count - 1;
  if Index = -1 then Index := 0
  else if Index > _MaxIndex then Index := _MaxIndex;
  List.Itemindex:=Index;
  SetButtons;
end;

//*************************************************************************
//Procedure: TNVBDblList.AutoInitialize
//Description:
//Author: SH
//History: 22.09.2001 14:47:26
//Description: Method to set variable and property values and create objects
//*************************************************************************
procedure TNVBDblList.AutoInitialize;
begin
  FVersion:=cComponentVersion;
  Caption:='';
  Height:=160;
  Width:=56;
  ParentColor:=true;
  FSourceOptions:=[AllowAdd,AllowRemove];
  FTargetOptions:=[AllowAdd,AllowRemove];  
//**********************************************************
  FAddAllItems := TBitBtn.Create(nil);
  FAddAllItems.Parent := Self;
  with FAddAllItems do begin
    Left  := 16;
    Top   := 56;
    Width   := cButtonSize;
    Height  := cButtonSize;
    Caption := '>>';
    OnClick:=AddAllItemsBtnClick;
  end;
//**********************************************************
  FAddItem := TBitBtn.Create(nil);
  FAddItem.Parent := Self;
  with FAddItem do begin
    Left := 16;
    Top  := 24;
    Width   := cButtonSize;
    Height  := cButtonSize;
    Caption := '>';
    OnClick:=AddItemBtnClick;
  end;
//**********************************************************
  FRemoveAllItems := TBitBtn.Create(nil);
  FRemoveAllItems.Parent := Self;
  with FRemoveAllItems do begin
    Left := 16;
    Top  := 120;
    Width   := cButtonSize;
    Height  := cButtonSize;
    Caption := '<<';
    Enabled := False;
    OnClick:=RemoveAllItemsBtnClick;
  end;
//**********************************************************
  FRemoveItem := TBitBtn.Create(nil);
  FRemoveItem.Parent := Self;
  with FRemoveItem do begin
    Left := 16;
    Top := 88;
    Width := cButtonSize;
    Height := cButtonSize;
    Caption := '<';
    Enabled := False;
    OnClick:=RemoveItemBtnClick;
  end;
  FSourceList := nil;
  FTargetList := nil;
  SetButtons;
end;

//*************************************************************************
//Procedure: TNVBDblList.Notification
//Author: SH
//History: 22.09.2001 14:47:26
//Description: Resets prop of component type if referenced component deleted
//*************************************************************************
procedure TNVBDblList.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove then Exit;
  if AComponent = FSourceList then FSourceList := nil;
  if AComponent = FTargetList then FTargetList := nil;
end;

//*************************************************************************
//Procedure: TNVBDblList.Loaded
//Author: SH
//History: 22.09.2001 14:47:26
//Description: we dont want to have a caption for this component.
//*************************************************************************
procedure TNVBDblList.Loaded;
begin
  inherited;
  Caption:='';
end;

{ Method to free any objects created by AutoInitialize }
procedure TNVBDblList.AutoDestroy;
begin
  if not (csDesigning in ComponentState) then begin
    if assigned(FSourceList) then begin
      FSourceList.OnDblClick:=FOriginalSrcDblClick;
      FSourceList.OnEnter:=FOriginalSrcEnter;
      FSourceList.OnMouseDown:=FOriginalSrcMouseDown;
      FSourceList.OnDragDrop:=FOriginalSrcDragDrop;
      FSourceList.OnDragOver:=FOriginalSrcDragOver;
    end;

    if assigned(FTargetList) then begin
      FTargetList.OnDblClick:=FOriginalTrgDblClick;
      FTargetList.OnEnter:=FOriginalTrgEnter;
      FTargetList.OnMouseDown:=FOriginalTrgMouseDown;
      FTargetList.OnDragDrop:=FOriginalTrgDragDrop;
      FTargetList.OnDragOver:=FOriginalTrgDragOver;
    end;
  end;
  FAddAllItems.Free;
  FAddItem.Free;
  FRemoveAllItems.Free;
  FRemoveItem.Free;
end;

{ Read method for property SourceList }

function TNVBDblList.GetSourceList : TListBox;
begin
  Result := FSourceList;
end;

{ Write method for property SourceList }
procedure TNVBDblList.SetSourceList(Value : TListBox);
begin
  if assigned(FTargetList) and
     assigned(Value) then begin
    if FTargetList.Name=Value.Name then begin
      MessageDlg('You can not set the Source-List and the Target-List to the same '+#13+#10+'Listbox !', mtWarning, [mbOK], 0);
      exit;
    end;
  end;
  FSourceList := Value;
  if Value=nil then exit;
  if not (csDesigning in ComponentState) then begin
// backup current eventhandlers
    FOriginalSrcMouseDown:=FSourceList.OnMouseDown;
    FOriginalSrcDblClick:=FSourceList.OnDblClick;
    FOriginalSrcEnter:=FSourceList.OnEnter;
    FOriginalSrcDragOver:=FSourceList.OnDragOver;
    FOriginalSrcDragDrop:=FSourceList.OnDragDrop;
// set new event handlers
    FSourceList.OnMouseDown:=SourceListBoxMouseDown;
    FSourceList.OnDblClick:=SourceListBoxDoubleClickEvent;
    FSourceList.OnEnter:=DoSrcListboxEnter;
    FSourceList.OnDragOver:=DoDragOver;
    FSourceList.OnDragDrop:=DoDragDrop;
  end;
end;

{ Read method for property TargetList }
function TNVBDblList.GetTargetList : TListBox;
begin
  Result := FTargetList;
end;

{ Write method for property TargetList }
procedure TNVBDblList.SetTargetList(Value : TListBox);
begin
  if assigned(FSourceList) and
     assigned(Value) then begin
    if FSourceList.Name=Value.Name then begin
      MessageDlg('You can not set the Source-List and the Target-List to the same '+#13+#10+'Listbox !', mtWarning, [mbOK], 0);
      exit;
    end;
  end;
  FTargetList := Value;
  if Value=nil then exit;
  if not (csDesigning in ComponentState) then begin
// backup original event handlers
    FOriginalTrgMouseDown:=FTargetList.OnMouseDown;
    FOriginalTrgDblClick:=FTargetList.OnDblClick;
    FOriginalTrgEnter:=FTargetList.OnEnter;
    FOriginalTrgDragOver:=FTargetList.OnDragOver;
    FOriginalTrgDragDrop:=FTargetList.OnDragDrop;
// set new event handlers.
    FTargetList.OnMouseDown:=TargetListBoxMouseDown;
    FTargetList.OnDblClick:=TargetListBoxDoubleClickEvent;
    FTargetList.OnEnter:=DoTrgListboxEnter;
    FTargetList.OnDragOver:=DoDragOver;
    FTargetList.OnDragDrop:=DoDragDrop;
  end;
end;

constructor TNVBDblList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoInitialize;
end;

destructor TNVBDblList.Destroy;
begin
  AutoDestroy;
  inherited Destroy;
end;

procedure TNVBDblList.SetVersion(const Value: string);
begin
  FVersion := cComponentVersion;
end;

procedure TNVBDblList.DoSrcListboxEnter(Sender: TObject);
begin
  SetButtons;
  if assigned(FOriginalSrcEnter) then FOriginalSrcEnter(self);
end;

procedure TNVBDblList.DoTrgListboxEnter(Sender: TObject);
begin
  SetButtons;
  if assigned(FOriginalTrgEnter) then FOriginalTrgEnter(self);
end;

procedure TNVBDblList.SetSourceOptions(const Value: TNVBDblListOptions);
begin
  FSourceOptions := Value;
end;

procedure TNVBDblList.SetTargetOptions(const Value: TNVBDblListOptions);
begin
  FTargetOptions := Value;
end;

{-----------------------------------------------------------------------------
  Procedure: DoCheckItem
  Author:    sam
  Date:      16-Dez-2005
  Arguments: _ItemText:string
             _Operation:TNVBOperationType
  Result:    boolean
  Description: the OnCheckItemEvent gives a possibility to the component user
               to do his own verification of the item.
               If the eventhandler returns true then the item gets added to the list.
-----------------------------------------------------------------------------}
function TNVBDblList.DoCheckItem(const _ItemText:string;const _Operation:TNVBOperationType):boolean;
begin
  result:=true;
  if assigned(FOnCheckItem) then result:=FOnCheckItem(self,_ItemText,_Operation);
end;

end.
