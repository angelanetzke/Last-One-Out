Type room
    roomRow As Integer
    roomColumn As Integer
    roomHeight As Integer
    roomWidth As Integer
    doorRow As Integer
    doorColumn As Integer
    partRow As Integer
    partColumn As Integer
End Type
Type location
    locationRow As Integer
    locationColumn As Integer
End Type
Type part
    isPartFound As Integer
    partRow As Integer
    partColumn As Integer
End Type
Type bandage
    isBandageFound As Integer
    bandageRow As Integer
    bandageColumn As Integer
End Type
Type ammo
    isAmmoFound As Integer
    ammoRow As Integer
    ammoColumn As Integer
End Type
Type inventory
    partCount As Integer
    bandageCount As Integer
    ammoCount As Integer
End Type
Type zombie
    isZombieDead As Integer
    zombieRow As Integer
    zombieColumn As Integer
End Type

Randomize Timer

'Assign constants
Dim Shared STATUS_CONTINUE As Integer
Dim Shared STATUS_WIN As Integer
Dim Shared STATUS_LOSE As Integer
Dim Shared STATUS_EXIT As Integer
STATUS_CONTINUE = 0
STATUS_WIN = 1
STATUS_LOSE = 2
STATUS_EXIT = 3
Dim Shared screenRowMin As Integer
Dim Shared screenRowMax As Integer
Dim Shared screenColumnMin As Integer
Dim Shared screenColumnMax As Integer
screenRowMin = 2
screenRowMax = 22
screenColumnMin = 1
screenColumnMax = 80
Dim arrowUp As String * 2
Dim arrowDown As String * 2
Dim arrowLeft As String * 2
Dim arrowRight As String * 2
arrowUp = Chr$(0) + Chr$(72)
arrowDown = Chr$(0) + Chr$(80)
arrowLeft = Chr$(0) + Chr$(75)
arrowRight = Chr$(0) + Chr$(77)
Dim Shared cornerULChar As Integer
Dim Shared cornerURChar As Integer
Dim Shared cornerLLChar As Integer
Dim Shared cornerLRChar As Integer
Dim Shared doorChar As Integer
Dim Shared wallHChar As Integer
Dim Shared wallVChar As Integer
Dim Shared blankChar As Integer
Dim Shared roomColor As Integer
cornerULChar = 201
cornerURChar = 187
cornerLLChar = 200
cornerLRChar = 188
doorChar = 206
wallHChar = 205
wallVChar = 186
blankChar = 32
roomColor = 6
Dim Shared heroChar As Integer
Dim Shared heroColor As Integer
heroChar = 1
heroColor = 3
Dim Shared partChar As Integer
Dim Shared partColor As Integer
Dim Shared partCount As Integer
partChar = 170
partColor = 11
partCount = 17
Dim Shared bandageChar As Integer
Dim Shared bandageColor As Integer
Dim Shared bandageCount As Integer
Dim Shared bandageHP As Integer
bandageChar = 247
bandageColor = 7
bandageCount = 10
bandageHP = 10
Dim Shared ammoChar As Integer
Dim Shared ammoColor As Integer
Dim Shared ammoCount As Integer
Dim Shared ammoMin As Integer
Dim Shared ammoMax As Integer
ammoChar = 173
ammoColor = 8
ammoCount = 7
ammoMin = 10
ammoMax = 20
Dim Shared teleporterChar As Integer
Dim Shared teleporterColor As Integer
teleporterChar = 227
teleporterColor = 14
Dim Shared zombieChar As Integer
Dim Shared zombieColor As Integer
Dim Shared zombieAggro As Integer
Dim Shared zombieCount As Integer
zombieChar = 90
zombieColor = 12
zombieAggro = 20
zombieCount = 8
Dim Shared textColor As Integer
textColor = 15
Dim Shared textColorSubdued As Integer
textColorSubdued = 7
Dim Shared clearText As String * 80
Dim Shared zombieHPMin As Integer
Dim Shared zombieHPMax As Integer
zombieHPMin = 10
zombieHPMax = 40
Dim Shared rifleDamageMin As Integer
Dim Shared rifleDamageMax As Integer
rifleDamageMin = 10
rifleDamageMax = 20
Dim Shared knifeDamageMin As Integer
Dim Shared knifeDamageMax As Integer
knifeDamageMin = 5
knifeDamageMax = 15
Dim Shared zombieDamageMin As Integer
Dim Shared zombieDamageMax As Integer
zombieDamageMin = 1
zombieDamageMax = 5

'Create world
Dim Shared stepsSinceLastMessage As Integer
stepsSinceLastMessage = 0
Dim Shared heroInventory As inventory
heroInventory.bandageCount = 3
heroInventory.ammoCount = 30
Dim Shared heroHP As Integer
heroHP = 50
Dim Shared heroHPMax As Integer
heroHPMax = 50
Dim Shared roomFieldWidth As Integer
Dim Shared roomFieldHeight As Integer
roomFieldWidth = 6
roomFieldHeight = 8
Dim Shared roomList(1 To roomFieldWidth * roomFieldHeight) As room
Dim Shared worldRowMin As Integer
Dim Shared worldRowMax As Integer
Dim Shared worldColumnMin As Integer
Dim Shared worldColumnMax As Integer
Dim roomIndices(1 To roomFieldWidth * roomFieldHeight - 1) As Integer
For i% = LBound(roomIndices) To UBound(roomIndices)
    roomIndices(i%) = i% + 1
Next i%
fieldPadding% = 2
initialStartRow% = fieldPadding% + 2
initialStartColumn% = fieldPadding% + 1
roomSpacing% = 17
roomSize% = 7
roomVariance% = 3
worldRowMin = 1
worldRowMax = roomSpacing% * (roomFieldHeight - 1) + roomSize% + 2 * roomVariance% + 2 * fieldPadding% + 2
worldColumnMin = 1
worldColumnMax = roomSpacing% * (roomFieldWidth - 1) + roomSize% + 2 * roomVariance% + 2 * fieldPadding% + 1
startRowMin% = initialStartRow%
startRowMax% = startRowMin% + roomVariance%
endRowMin% = startRowMax% + roomSize%
endRowMax% = endRowMin% + roomVariance%
startColumnMin% = initialStartColumn%
startColumnMax% = startColumnMin% + roomVariance%
endColumnMin% = startColumnMax% + roomSize%
endColumnMax% = endColumnMin% + roomVariance%
For i% = 0 To roomFieldHeight - 1
    For j% = 0 To roomFieldWidth - 1
        thisIndex% = roomFieldWidth * i% + j% + 1
        thisStartRow% = Int(Rnd * (startRowMax% - startRowMin% + 1)) + startRowMin%
        thisEndRow% = Int(Rnd * (endRowMax% - endRowMin% + 1)) + endRowMin%
        thisStartColumn% = Int(Rnd * (startColumnMax% - startColumnMin% + 1)) + startColumnMin%
        thisEndColumn% = Int(Rnd * (endColumnMax% - endColumnMin% + 1)) + endColumnMin%
        roomList(thisIndex%).roomRow = thisStartRow%
        roomList(thisIndex%).roomColumn = thisStartColumn%
        roomList(thisIndex%).roomHeight = thisEndRow% - thisStartRow% + 1
        roomList(thisIndex%).roomWidth = thisEndColumn% - thisStartColumn% + 1
        roomList(thisIndex%).doorRow = Int(Rnd * (thisEndRow% - thisStartRow% - 1)) + 1
        roomList(thisIndex%).doorColumn = Int(Rnd * (thisEndColumn% - thisStartColumn% - 1)) + 1
        startColumnMin% = startColumnMin% + roomSpacing%
        startColumnMax% = startColumnMax% + roomSpacing%
        endColumnMin% = endColumnMin% + roomSpacing%
        endColumnMax% = endColumnMax% + roomSpacing%
    Next j%
    startRowMin% = startRowMin% + roomSpacing%
    startRowMax% = startRowMax% + roomSpacing%
    endRowMin% = endRowMin% + roomSpacing%
    endRowMax% = endRowMax% + roomSpacing%
    startColumnMin% = initialStartColumn%
    startColumnMax% = startColumnMin% + roomVariance%
    endColumnMin% = startColumnMax% + roomSize%
    endColumnMax% = endColumnMin% + roomVariance%
Next i%
For i% = LBound(roomIndices) To UBound(roomIndices)
    j% = Int(Rnd * (UBound(roomIndices) - LBound(roomIndices) + 1)) + LBound(roomIndices)
    temp% = roomIndices(i%)
    roomIndices(i%) = roomIndices(j%)
    roomIndices(j%) = temp%
Next i%
Dim Shared partList(partCount) As part
Dim thisLocation As location
For i% = 1 To partCount
    Call getLocationInRoom(roomList(roomIndices(i%)), thisLocation)
    partList(i%).partRow = thisLocation.locationRow
    partList(i%).partColumn = thisLocation.locationColumn
Next i%
Dim Shared bandageList(bandageCount) As bandage
For i% = 1 To bandageCount
    Call getLocationInRoom(roomList(roomIndices(i% + partCount)), thisLocation)
    bandageList(i%).bandageRow = thisLocation.locationRow
    bandageList(i%).bandageColumn = thisLocation.locationColumn
Next i%
Dim Shared ammoList(ammoCount) As ammo
For i% = 1 To ammoCount
    Call getLocationInRoom(roomList(roomIndices(i% + partCount + bandageCount)), thisLocation)
    ammoList(i%).ammoRow = thisLocation.locationRow
    ammoList(i%).ammoColumn = thisLocation.locationColumn
Next i%
For i% = LBound(roomIndices) To UBound(roomIndices)
    j% = Int(Rnd * (UBound(roomIndices) - LBound(roomIndices) + 1)) + LBound(roomIndices)
    temp% = roomIndices(i%)
    roomIndices(i%) = roomIndices(j%)
    roomIndices(j%) = temp%
Next i%
Dim Shared zombieList(zombieCount) As zombie
For i% = 1 To zombieCount
    Call getLocationInRoom(roomList(roomIndices(i%)), thisLocation)
    zombieList(i%).zombieRow = thisLocation.locationRow
    zombieList(i%).zombieColumn = thisLocation.locationColumn
Next i%
Dim Shared rowOffset As Integer
Dim Shared columnOffset As Integer
rowOffsetMin% = screenRowMax - worldRowMax
rowOffsetMax% = 0
columnOffsetMin% = screenColumnMax - worldColumnMax
columnOffsetMax% = 0
rowOffset = rowOffsetMax%
columnOffset = columnOffsetMax%
Dim Shared oldRowOffset As Integer
Dim Shared oldColumnOffset As Integer
oldRowOffset = 0
oldColumnOffset = 0
Dim Shared heroRow As Integer
Dim Shared heroColumn As Integer
heroRow = roomList(1).roomRow + 1
heroColumn = roomList(1).roomColumn + 1
Dim Shared teleporterRow As Integer
Dim Shared teleporterColumn As Integer
teleporterRow = roomList(1).roomRow + 2
teleporterColumn = roomList(1).roomColumn + 1


'Title screen
lastMode% = -1
currentMode% = 0
Do While currentMode% <> 2
    If currentMode% = 0 And lastMode% <> 0 Then
        Cls
        Color heroColor, 0
        Print
        Print
        Print
        Print
        Print "      #       #    #### #####      ###  #   # #####      ###  #   # #####      "
        Print "      #      # #  #       #       #   # ##  # #         #   # #   #   #        "
        Print "      #     #####  ###    #       #   # # # # #####     #   # #   #   #        "
        Print "      #     #   #     #   #       #   # #  ## #         #   # #   #   #        "
        Print "      ##### #   # ####    #        ###  #   # #####      ###   ###    #        "
        Color textColor, 0
        Print
        Print
        Print "                          Press H to show help."
        Print "                          Press any other key to start game."
        Print
        Print
        Color zombieColor, 0
        Print "        Z               Z                                      Z             Z "
        Print "             Z                     Z                     Z"
        Print "                           Z                                       Z"
        Color heroColor, 0
        Locate 18, 40
        Print Chr$(heroChar)
        Color textColor, 0
        keyPressed$ = ""
        Do While Len(keyPressed$) = 0
            keyPressed$ = InKey$
        Loop
        If keyPressed$ = "H" Or keyPressed$ = "h" Then
            currentMode% = 1
        Else
            currentMode% = 2
        End If
        lastMode% = 0
    End If
    If currentMode% = 1 And lastMode% <> 1 Then
        Cls
        Call showHelp
        Do While Len(InKey$) = 0
        Loop
        currentMode% = 0
        lastMode% = 1
    End If
Loop

'Game loop
Cls
Color textColor, 0
Locate 1, 1
Print "Find all " + LTrim$(Str$(partCount)) + " parts ("
Color partColor, 0
Locate 1, 20
Print Chr$(partChar)
Color textColor, 0
Locate 1, 21
Print ") and return to the teleporter ("
Color teleporterColor, 0
Locate 1, 53
Print Chr$(teleporterChar)
Color textColor, 0
Locate 1, 54
Print ")"
Dim Shared gameStatus As Integer
gameStatus = STATUS_CONTINUE
Dim Shared doRedraw As Integer
doRedraw = 1
Do While gameStatus = STATUS_CONTINUE
    If doRedraw = 1 Then
        Call eraseOld
        Call moveZombies
        Call drawNew
        Color textColor, 0
        Locate screenRowMax + 1, 1
        Print clearText
        Locate screenRowMax + 1, 2
        Print "H)elp B)andage Q)uit"
        Locate screenRowMax + 1, 30
        Print "Parts:"
        Locate screenRowMax + 1, 37
        Print Using "##"; heroInventory.partCount
        Locate screenRowMax + 1, 41
        Print "Bandages:"
        Locate screenRowMax + 1, 51
        Print Using "##"; heroInventory.bandageCount
        Locate screenRowMax + 1, 55
        Print "Ammo:"
        Locate screenRowMax + 1, 61
        Print Using "###"; heroInventory.ammoCount
        Locate screenRowMax + 1, 66
        Print "HP:"
        Locate screenRowMax + 1, 71
        Print Using "##"; heroHP
        doRedraw = 0
        Call pickUpItems
        If heroInventory.partCount = partCount And teleporterColor <= 25 Then teleporterColor = teleporterColor + 16
        For i% = 1 To UBound(zombieList)
            If zombieList(i%).isZombieDead = 0 And zombieList(i%).zombieRow = heroRow And zombieList(i%).zombieColumn = heroColumn Then
                Call fightZombies(i%)
                Do While Len(InKey$) = 0
                Loop
                Cls
                doRedraw = 1
            End If
        Next i%
    End If
    keyPressed$ = InKey$
    Select Case keyPressed$
        Case arrowUp
            If heroRow + rowOffset > screenRowMin Then
                If isTraversible(heroRow - 1, heroColumn) = 1 Then
                    Call update
                    If heroRow + rowOffset < screenRowMax / 2 - 2 Then rowOffset = rowOffset + 1
                    If rowOffset > rowOffsetMax% Then rowOffset = rowOffsetMax%
                    heroRow = heroRow - 1
                    If heroRow < worldRowMin Then heroRow = worldRowMin
                End If
            End If
        Case arrowDown
            If heroRow + rowOffset < screenRowMax Then
                If isTraversible(heroRow + 1, heroColumn) = 1 Then
                    Call update
                    If heroRow + rowOffset > screenRowMax / 2 + 2 Then rowOffset = rowOffset - 1
                    If rowOffset < rowOffsetMin% Then rowOffset = rowOffsetMin%
                    heroRow = heroRow + 1
                    If heroRow > worldRowMax Then heroRow = worldRowMax
                End If
            End If
        Case arrowLeft
            If heroColumn + columnOffset > screenColumnMin Then
                If isTraversible(heroRow, heroColumn - 1) = 1 Then
                    Call update
                    If heroColumn + columnOffset < screenColumnMax / 2 - 2 Then columnOffset = columnOffset + 1
                    If columnOffset > columnOffsetMax% Then columnOffset = columnOffsetMax%
                    heroColumn = heroColumn - 1
                    If heroColumn < worldColumnMin Then heroColumn = worldColumnMin
                End If
            End If
        Case arrowRight
            If heroColumn + columnOffset < screenColumnMax Then
                If isTraversible(heroRow, heroColumn + 1) = 1 Then
                    Call update
                    If heroColumn + columnOffset > screenColumnMax / 2 + 2 Then columnOffset = columnOffset - 1
                    If columnOffset < columnOffsetMin% Then columnOffset = columnOffsetMin%
                    heroColumn = heroColumn + 1
                    If heroColumn > worldColumnMax Then heroColumn = worldColumnMax
                End If
            End If
        Case "H", "h"
            Cls
            Call showHelp
            Do While Len(InKey$) = 0
            Loop
            Cls
            doRedraw = 1
        Case "B", "b"
            If heroInventory.bandageCount > 0 Then
                If heroHP < heroHPMax Then
                    Call printMessage("You use a bandage and gain " + Str$(bandageHP) + " HP.")
                    heroInventory.bandageCount = heroInventory.bandageCount - 1
                    heroHP = heroHP + bandageHP
                    If heroHP > heroHPMax Then heroHP = heroHPMax
                    doRedraw = 1
                Else
                    Call printMessage("You are already at full HP.")
                End If
            Else
                Call printMessage("You have no bandages to use.")
            End If
        Case "Q", "q"
            Call printMessage("Ready to quit? Y/N")
            quitPromptKeyPressed$ = ""
            Do Until Len(quitPromptKeyPressed$) > 0
                quitPromptKeyPressed$ = InKey$
                If quitPromptKeyPressed$ = "Y" Or quitPromptKeyPressed$ = "y" Then gameStatus = STATUS_EXIT
            Loop
            Locate 1, 1
            Print clearText
    End Select
    If heroRow = teleporterRow And heroColumn = teleporterColumn And heroInventory.partCount = partCount Then
        gameStatus = STATUS_WIN
    End If
Loop

If gameStatus = STATUS_WIN Then
    Cls
    Color heroColor, 0
    Print "You escape in the teleporter!"
    Print
    Print "        ########"
    Print "        #      #"
    Print "        # #  # #"
    Print "        #      #"
    Print "        # #### #"
    Print "        #  ##  #"
    Print "        #      #"
    Print "        ########"
    Color 7, 0
    End
End If
If gameStatus = STATUS_LOSE Then
    Cls
    Color zombieColor, 0
    Print "        You died!"
    Print
    Print "        #########"
    Print "        #       #"
    Print "        #  RIP  #"
    Print "        #       #"
    Print "        #       #"
    Print "        #       #"
    Print "        #       #"
    Print "        #       #"
    Color 7, 0
    End
End If

'Clean up before exit
Cls
Color 7, 0
End

Sub update
    oldRowOffset = rowOffset
    oldColumnOffset = columnOffset
    Locate heroRow + rowOffset, heroColumn + columnOffset
    Print " "
    Call handleMessageClearing
    doRedraw = 1
End Sub

Function isOnScreenNew (worldRow As Integer, worldColumn As Integer)
    if screenRowMin <= worldRow + rowOffSet and worldRow + rowOffset <= screenRowMax _
        and screenColumnMin <= worldColumn + columnOffset and worldColumn + columnOffset <= screenColumnMax then
        isOnScreenNew = 1
    Else
        isOnScreenNew = 0
    End If
End Function

Function isOnScreenOld (worldRow As Integer, worldColumn As Integer)
    if screenRowMin <= worldRow + oldRowOffset and worldRow + oldRowOffset <= screenRowMax _
        and screenColumnMin <= worldColumn + oldColumnOffset and worldColumn + oldColumnOffset <= screenColumnMax then
        isOnScreenOld = 1
    Else
        isOnScreenOld = 0
    End If
End Function


Sub eraseOld
    For i% = LBound(roomList) To UBound(roomList)
        Call eraseRoom(roomList(i%))
    Next i%
    For i% = 1 To UBound(partList)
        If isOnScreenOld(partList(i%).partRow, partList(i%).partColumn ) = 1 _
                and partList(i%).isPartFound = 0 and (rowOffset <> oldRowOffset or columnOffset <> oldColumnOffset) Then
            Locate partList(i%).partRow + oldRowOffset, partList(i%).partColumn + oldColumnOffset
            Print " "
        End If
    Next i%
    For i% = 1 To UBound(bandageList)
        If isOnScreenOld(bandageList(i%).bandageRow, bandageList(i%).bandageColumn) = 1 _
                and bandageList(i%).isBandageFound = 0 and (rowOffset <> oldRowOffset or columnOffset <> oldColumnOffset) Then
            Locate bandageList(i%).bandageRow + oldRowOffset, bandageList(i%).bandageColumn + oldColumnOffset
            Print " "
        End If
    Next i%
    For i% = 1 To UBound(ammoList)
        If isOnScreenOld(ammoList(i%).ammoRow, ammoList(i%).ammoColumn) = 1 _
            and ammoList(i%).isAmmoFound = 0 and (rowOffset <> oldRowOffset or columnOffset <> oldColumnOffset) Then
            Locate ammoList(i%).ammoRow + oldRowOffset, ammoList(i%).ammoColumn + oldColumnOffset
            Print " "
        End If
    Next i%
    For i% = 1 To UBound(zombieList)
        If isOnScreenOld(zombieList(i%).zombieRow, zombieList(i%).zombieColumn) = 1 _
            and zombieList(i%).isZombieDead = 0 and (rowOffset <> oldRowOffset or columnOffset <> oldColumnOffset) Then
            Locate zombieList(i%).zombieRow + oldRowOffset, zombieList(i%).zombieColumn + oldColumnOffset
            Print " "
        End If
    Next i%
    if isOnScreenOld(teleporterRow, teleporterColumn) = 1 _
        and (rowOffset <> oldRowOffset or columnOffset <> oldColumnOffset) Then
        Locate teleporterRow + oldRowOffset, teleporterColumn + oldColumnOffset
        Print " "
    End If
End Sub

Sub drawNew
    For i% = LBound(roomList) To UBound(roomList)
        Call drawRoom(roomList(i%))
    Next i%
    Color partColor, 0
    For i% = 1 To UBound(partList)
        If isOnScreenNew(partList(i%).partRow, partList(i%).partColumn) = 1 _
            and partList(i%).isPartFound = 0 Then
            Locate partList(i%).partRow + rowOffset, partList(i%).partColumn + columnOffset
            Print Chr$(partChar)
        End If
    Next i%
    Color bandageColor, 0
    For i% = 1 To UBound(bandageList)
        If isOnScreenNew(bandageList(i%).bandageRow, bandageList(i%).bandageColumn) = 1 _
            and bandageList(i%).isBandageFound = 0 Then
            Locate bandageList(i%).bandageRow + rowOffset, bandageList(i%).bandageColumn + columnOffset
            Print Chr$(bandageChar)
        End If
    Next i%
    Color ammoColor, 0
    For i% = 1 To UBound(ammoList)
            If isOnScreenNew(ammoList(i%).ammoRow, ammoList(i%).ammoColumn) = 1 _
                and ammoList(i%).isAmmoFound = 0 Then
            Locate ammoList(i%).ammoRow + rowOffset, ammoList(i%).ammoColumn + columnOffset
            Print Chr$(ammoChar)
        End If
    Next i%
    Color zombieColor, 0
    For i% = 1 To UBound(zombieList)
            If isOnScreenNew(zombieList(i%).zombieRow, zombieList(i%).zombieColumn) = 1 _
                and zombieList(i%).isZombieDead = 0 Then
            Locate zombieList(i%).zombieRow + rowOffset, zombieList(i%).zombieColumn + columnOffset
            Print Chr$(zombieChar)
        End If
    Next i%
    Color teleporterColor, 0
    If isOnScreenNew(teleporterRow, teleporterColumn) = 1 Then
        Locate teleporterRow + rowOffset, teleporterColumn + columnOffset
        Print Chr$(teleporterChar)
    End If
    Color heroColor, 0
    Locate heroRow + rowOffset, heroColumn + columnOffset
    Print Chr$(heroChar)
End Sub


Sub drawRoom (currentRoom As room)
    Color roomColor, 0
    roomRowMax% = currentRoom.roomRow + currentRoom.roomHeight - 1
    roomColumnMax% = currentRoom.roomColumn + currentRoom.roomWidth - 1
    If isOnScreenNew(currentRoom.roomRow, currentRoom.roomColumn) = 1 Then
        Locate currentRoom.roomRow + rowOffset, currentRoom.roomColumn + columnOffset
        Print Chr$(cornerULChar)
    End If
    If isOnScreenNew(roomRowMax%, currentRoom.roomColumn) = 1 Then
        Locate roomRowMax% + rowOffset, currentRoom.roomColumn + columnOffset
        Print Chr$(cornerLLChar)
    End If
    If isOnScreenNew(currentRoom.roomRow, roomColumnMax%) = 1 Then
        Locate currentRoom.roomRow + rowOffset, roomColumnMax% + columnOffset
        Print Chr$(cornerURChar)
    End If
    If isOnScreenNew(roomRowMax%, roomColumnMax%) = 1 Then
        Locate roomRowMax% + rowOffset, roomColumnMax% + columnOffset
        Print Chr$(cornerLRChar)
    End If
    For i% = currentRoom.roomRow + 1 To roomRowMax% - 1
        If i% - currentRoom.roomRow = currentRoom.doorRow Then
            If isOnScreenNew(i%, currentRoom.roomColumn) = 1 Then
                Locate i% + rowOffset, currentRoom.roomColumn + columnOffset
                Print Chr$(doorChar)
            End If
            If isOnScreenNew(i%, roomColumnMax%) = 1 Then
                Locate i% + rowOffset, roomColumnMax% + columnOffset
                Print Chr$(doorChar)
            End If
        Else
            If isOnScreenNew(i%, currentRoom.roomColumn) = 1 Then
                Locate i% + rowOffset, currentRoom.roomColumn + columnOffset
                Print Chr$(wallVChar)
            End If
            If isOnScreenNew(i%, roomColumnMax%) = 1 Then
                Locate i% + rowOffset, roomColumnMax% + columnOffset
                Print Chr$(wallVChar)
            End If
        End If
    Next i%
    For i% = currentRoom.roomColumn + 1 To roomColumnMax% - 1
        If i% - currentRoom.roomColumn = currentRoom.doorColumn Then
            If isOnScreenNew(currentRoom.roomRow, i%) = 1 Then
                Locate currentRoom.roomRow + rowOffset, i% + columnOffset
                Print Chr$(doorChar)
            End If
            If isOnScreenNew(roomRowMax%, i%) = 1 Then
                Locate roomRowMax% + rowOffset, i% + columnOffset
                Print Chr$(doorChar)
            End If
        Else
            If isOnScreenNew(currentRoom.roomRow, i%) = 1 Then
                Locate currentRoom.roomRow + rowOffset, i% + columnOffset
                Print Chr$(wallHChar)
            End If
            If isOnScreenNew(roomRowMax%, i%) = 1 Then
                Locate roomRowMax% + rowOffset, i% + columnOffset
                Print Chr$(wallHChar)
            End If
        End If
    Next i%
End Sub

Sub eraseRoom (currentRoom As room)
    roomRowMax% = currentRoom.roomRow + currentRoom.roomHeight - 1
    roomColumnMax% = currentRoom.roomColumn + currentRoom.roomWidth - 1
    If rowOffset > oldRowOffset Then
        For i% = currentRoom.roomColumn To roomColumnMax%
            If isOnScreenNew(currentRoom.roomRow - 1, i%) Then
                Locate currentRoom.roomRow - 1 + rowOffset, i% + columnOffset
                Print " "
            End If
            If isOnScreenNew(roomRowMax% - 1, i%) Then
                Locate roomRowMax% - 1 + rowOffset, i% + columnOffset
                Print " "
            End If
        Next i%
    End If
    If rowOffset < oldRowOffset Then
        For i% = currentRoom.roomColumn To roomColumnMax%
            If isOnScreenNew(currentRoom.roomRow + 1, i%) Then
                Locate currentRoom.roomRow + 1 + rowOffset, i% + columnOffset
                Print " "
            End If
            If isOnScreenNew(roomRowMax% + 1, i%) Then
                Locate roomRowMax% + 1 + rowOffset, i% + columnOffset
                Print " "
            End If
        Next i%
    End If
    If columnOffset > oldColumnOffset Then
        For i% = currentRoom.roomRow To roomRowMax%
            If isOnScreenNew(i%, currentRoom.roomColumn - 1) Then
                Locate i% + rowOffset, currentRoom.roomColumn - 1 + columnOffset
                Print " "
            End If
            If isOnScreenNew(i%, roomColumnMax% - 1) Then
                Locate i% + rowOffset, roomColumnMax% - 1 + columnOffset
                Print " "
            End If
        Next i%
    End If
    If columnOffset < oldColumnOffset Then
        For i% = currentRoom.roomRow To roomRowMax%
            If isOnScreenNew(i%, currentRoom.roomColumn + 1) Then
                Locate i% + rowOffset, currentRoom.roomColumn + 1 + columnOffset
                Print " "
            End If
            If isOnScreenNew(i%, roomColumnMax% + 1) Then
                Locate i% + rowOffset, roomColumnMax% + 1 + columnOffset
                Print " "
            End If
        Next i%
    End If
End Sub

Sub getLocationInRoom (currentRoom As room, result As location)
    Dim selectedLocation As location
    selectedLocation.locationRow = Int(Rnd * (currentRoom.roomHeight - 2)) + currentRoom.roomRow + 1
    selectedLocation.locationColumn = Int(Rnd * (currentRoom.roomWidth - 2)) + currentRoom.roomColumn + 1
    result = selectedLocation
End Sub

Sub moveZombies
    Dim nextDeltas(1 To 4) As location
    nextDeltas(1).locationRow = -1
    nextDeltas(1).locationColumn = 0
    nextDeltas(2).locationRow = 1
    nextDeltas(2).locationColumn = 0
    nextDeltas(3).locationRow = 0
    nextDeltas(3).locationColumn = -1
    nextDeltas(4).locationRow = 0
    nextDeltas(4).locationColumn = 1
    Dim queue(1 To (2 * zombieAggro + 1) * (2 * zombieAggro + 1) + 1) As location
    For i% = 1 To zombieCount
        If zombieList(i%).isZombieDead = 0 Then
            redim visited(zombieList(i%).zombieRow - zombieAggro To zombieList(i%).zombieRow + zombieAggro, _
                zombieList(i%).zombieColumn - zombieAggro To zombieList(i%).zombieColumn + zombieAggro) As Integer
            redim parents(zombieList(i%).zombieRow - zombieAggro To zombieList(i%).zombieRow + zombieAggro, _
                zombieList(i%).zombieColumn - zombieAggro To zombieList(i%).zombieColumn + zombieAggro) As location
            If isOnScreenNew(zombieList(i%).zombieRow, zombieList(i%).zombieColumn) = 1 Then
                For deltaRow% = -1 * zombieAggro To zombieAggro
                    For deltaColumn% = -1 * zombieAggro To zombieAggro
                        visited(zombieList(i%).zombieRow + deltaRow%, zombieList(i%).zombieColumn + deltaColumn%) = 0
                        parents(zombieList(i%).zombieRow + deltaRow%, zombieList(i%).zombieColumn + deltaColumn%).locationRow = -1
                        parents(zombieList(i%).zombieRow + deltaRow%, zombieList(i%).zombieColumn + deltaColumn%).locationColumn = -1
                    Next deltaColumn%
                Next deltaRow%
                queueFirst% = 1
                queueNext% = 2
                queue(queueFirst%).locationRow = zombieList(i%).zombieRow
                queue(queueFirst%).locationColumn = zombieList(i%).zombieColumn
                isHeroFound% = 0
                Do While queueNext% - queueFirst% >= 1
                    Dim current As location
                    current = queue(queueFirst%)
                    queueFirst% = queueFirst% + 1
                    currentRow% = current.locationRow
                    currentColumn% = current.locationColumn
                    visited(currentRow%, currentColumn%) = 1
                    If isHeroFound% = 0 Then
                        For j% = 1 To 4
                            nextRow% = currentRow% + nextDeltas(j%).locationRow
                            nextColumn% = currentColumn% + nextDeltas(j%).locationColumn
                            If canMove(zombieList(i%), nextRow%, nextColumn%) = 1 Then
                            If parents(nextRow%, nextColumn%).locationRow = -1 And parents(nextRow%, nextColumn%).locationColumn = -1 _
                                And visited(nextRow%, nextColumn%) = 0 Then
                                    queue(queueNext%).locationRow = nextRow%
                                    queue(queueNext%).locationColumn = nextColumn%
                                    queueNext% = queueNext% + 1
                                    parents(nextRow%, nextColumn%).locationRow = currentRow%
                                    parents(nextRow%, nextColumn%).locationColumn = currentColumn%
                                    If nextRow% = heroRow And nextColumn% = heroColumn Then isHeroFound% = 1
                                End If
                            End If
                        Next j%
                    End If
                Loop
                If isHeroFound% = 1 Then
                    Dim firstStep As location
                    firstStep.locationRow = heroRow
                    firstStep.locationColumn = heroColumn
                Do Until parents(firstStep.locationRow, firstStep.locationColumn).locationRow = zombieList(i%).zombieRow _
                    And parents(firstStep.locationRow, firstStep.locationColumn).locationColumn = zombieList(i%).zombieColumn
                        parentRow% = parents(firstStep.locationRow, firstStep.locationColumn).locationRow
                        parentColumn% = parents(firstStep.locationRow, firstStep.locationColumn).locationColumn
                        firstStep.locationRow = parentRow%
                        firstStep.locationColumn = parentColumn%
                    Loop
                    If isOnScreenNew(zombieList(i%).zombieRow, zombieList(i%).zombieColumn) = 1 Then
                        Locate zombieList(i%).zombieRow + rowOffset, zombieList(i%).zombieColumn + columnOffset
                        Print " "
                    End If
                    zombieList(i%).zombieRow = firstStep.locationRow
                    zombieList(i%).zombieColumn = firstStep.locationColumn
                End If
            End If
        End If
    Next i%
End Sub

Sub fightZombies (zombieIndex As Integer)
    Cls
    Dim zombieRow(1 To 9) As Integer
    Dim zombieHP(1 To 9) As Integer
    remainingZombieCount% = 0
    retreatsRemaining% = 3
    retreatDistance% = 2
    For i% = 1 To 9
        Color zombieColor, 0
        roll% = Int(Rnd * 4)
        If roll% = 0 Then
            zombieRow(i%) = -1
        Else
            zombieRow(i%) = Int(Rnd * ((screenRowMax - 5) - 2 + 1)) + 2
            zombieHP(i%) = Int(Rnd * (zombieHPMax - zombieHPMin + 1)) + zombieHPMin
            Locate zombieRow(i%), i% + 1
            Print Chr$(zombieChar)
        End If
        If zombieRow(i%) >= 1 Then remainingZombieCount% = remainingZombieCount% + 1
    Next i%
    Color textColorSubdued, 0
    dataStartColumn% = 20
    interfaceStartColumn% = 40
    Dim interfaceLines(3 To screenRowMax - 5) As String * 39
    Locate 1, dataStartColumn%
    Print "Column Distance HP"
    fightStatus% = STATUS_CONTINUE
    Do While fightStatus% = STATUS_CONTINUE
        For i% = 1 To 9
            If zombieRow(i%) < 0 Then
                Color textColorSubdued, 0
            Else
                Color textColor, 0
            End If
            Locate 1, i% + 1
            Print LTrim$(Str$(i%))
        Next i%
        Call printZombieData(dataStartColumn%, zombieRow(), zombieHP())
        Call printFightInventory(interfaceStartColumn%, retreatsRemaining%)
        Color heroColor, 0
        Locate screenRowMax, 6
        Print Chr$(heroChar)
        selectedZombie% = -1
        Do While selectedZombie% = -1
            Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "1-9: attack zombie")
            Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "F: move forward")
            Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "B: use bandage")
            Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "R: retreat")
            pressedKey$ = ""
            Do While Len(pressedKey$) = 0
                pressedKey$ = InKey$
            Loop
            Call clearFightInterface(interfaceLines())
            If "1" <= pressedKey$ And pressedKey$ <= "9" Then
                selectedZombie% = Asc(pressedKey$) - Asc("0")
                If zombieRow(selectedZombie%) < 1 Then
                    Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "That zombie does not exist.")
                    selectedZombie% = -1
                ElseIf screenRowMax - zombieRow(selectedZombie%) > 1 And heroInventory.ammoCount < 1 Then
                    Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "Out of ammo. Move forward to use knife.")
                    selectedZombie% = -1
                End If
            End If
            If pressedKey$ = "B" Or pressedKey$ = "b" Then
                If heroInventory.bandageCount > 0 Then
                    If heroHP < heroHPMax Then
                        Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "You use a bandage and gain " + Str$(bandageHP) + " HP.")
                        heroInventory.bandageCount = heroInventory.bandageCount - 1
                        heroHP = heroHP + bandageHP
                        If heroHP > heroHPMax Then heroHP = heroHPMax
                    Else
                        Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "You are already at full HP.")
                    End If
                Else
                    Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "You have no bandages to use.")
                End If
                Call printFightInventory(interfaceStartColumn%, retreatsRemaining%)
            End If
            If pressedKey$ = "F" Or pressedKey$ = "f" Then
                For i% = 1 To 9
                    If 1 <= zombieRow(i%) And zombieRow(i%) < screenRowMax - 1 Then
                        Locate zombieRow(i%), i% + 1
                        Print " "
                        zombieRow(i%) = zombieRow(i%) + 1
                        Color zombieColor, 0
                        Locate zombieRow(i%), i% + 1
                        Print Chr$(zombieChar)
                    End If
                Next i%
                Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "You move forward.")
                Call printZombieData(dataStartColumn%, zombieRow(), zombieHP())
            End If
            If pressedKey$ = "R" Or pressedKey$ = "r" Then
                If retreatsRemaining% > 0 Then
                    For i% = 1 To 9
                        If zombieRow(i%) >= 1 Then
                            Locate zombieRow(i%), i% + 1
                            Print " "
                            zombieRow(i%) = zombieRow(i%) - retreatDistance%
                            If zombieRow(i%) < 2 Then zombieRow(i%) = 2
                            Color zombieColor, 0
                            Locate zombieRow(i%), i% + 1
                            Print Chr$(zombieChar)
                        End If
                    Next i%
                    retreatsRemaining% = retreatsRemaining% - 1
                    Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "You move away.")
                Else
                    Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "You can no longer retreat.")
                End If
                Call printZombieData(dataStartColumn%, zombieRow(), zombieHP())
                Call printFightInventory(interfaceStartColumn%, retreatsRemaining%)
            End If
        Loop
        Call clearFightInterface(interfaceLines())
        If zombieRow(selectedZombie%) = screenRowMax - 1 Then
            roll% = Int(Rnd * 10)
            If roll% = 0 Then
                Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "You miss the zombie with your knife.")
            Else
                roll% = Int(Rnd * (knifeDamageMax - knifeDamageMin) + 1) + knifeDamageMin
                zombieHP(selectedZombie%) = zombieHP(selectedZombie%) - roll%
                Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "You deal " + Str$(roll%) + " damage with your knife.")
            End If
        Else
            heroInventory.ammoCount = heroInventory.ammoCount - 1
            roll% = Int(Rnd * 6)
            If roll% = 0 Then
                Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "You miss the zombie with your rifle.")
            Else
                roll% = Int(Rnd * (rifleDamageMax - rifleDamageMin) + 1) + rifleDamageMin
                zombieHP(selectedZombie%) = zombieHP(selectedZombie%) - roll%
                Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "You deal " + Str$(roll%) + " damage with your rifle.")
            End If
        End If
        If zombieHP(selectedZombie%) <= 0 Then
            Locate zombieRow(selectedZombie%), selectedZombie% + 1
            Print " "
            zombieRow(selectedZombie%) = -1
            remainingZombieCount% = remainingZombieCount% - 1
        End If
        For i% = 1 To 9
            If 1 <= zombieRow(i%) And zombieRow(i%) < screenRowMax - 1 Then
                Locate zombieRow(i%), i% + 1
                Print " "
                zombieRow(i%) = zombieRow(i%) + 1
                Color zombieColor, 0
                Locate zombieRow(i%), i% + 1
                Print Chr$(zombieChar)
            End If
        Next i%
        For i% = 1 To 9
            If zombieRow(i%) = screenRowMax - 1 Then
                roll% = Int(Rnd * 2)
                If roll% = 0 Then
                    Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "A zombie misses you.")
                Else
                    roll% = Int(Rnd * (zombieDamageMax - zombieDamageMin + 1)) + zombieDamageMin
                    heroHP = heroHP - roll%
                    Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "A zombie deals " + Str$(roll%) + " damage to you.")
                End If
            End If
        Next i%
        If remainingZombieCount% = 0 Then
            fightStatus% = STATUS_WIN
        ElseIf heroHP <= 0 Then
            heroHP = 0
            fightStatus% = STATUS_LOSE
        End If
    Loop
    Call printZombieData(dataStartColumn%, zombieRow(), zombieHP())
    Call printFightInventory(interfaceStartColumn%, retreatsRemaining%)
    If fightStatus% = STATUS_WIN Then
        Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "You win!")
        Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "Press any key to continue.")
    ElseIf fightStatus% = STATUS_LOSE Then
        Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "You have died!")
        Call printToFightInterface(interfaceStartColumn%, interfaceLines(), "Press any key to continue.")
        gameStatus = STATUS_LOSE
    End If
    Color textColor, 0
    zombieList(zombieIndex).isZombieDead = 1
End Sub

Sub printZombieData (dataStartColumn As Integer, zombieRow() As Integer, zombieHP() As Integer)
    Color textColorSubdued, 0
    For i% = 1 To 9
        If zombieRow(i%) >= 1 Then
            zombieDistance% = screenRowMax - zombieRow(i%)
            If (zombieDistance% < 2) Then
                Color zombieColor, 0
            Else
                Color textColor, 0
            End If
            Locate i% + 1, dataStartColumn
            Print LTrim$(Str$(i%))
            Locate i% + 1, dataStartColumn + 7
            Print Using "##"; zombieDistance%
            Locate i% + 1, dataStartColumn + 16
            Print Using "##"; zombieHP(i%)
        Else
            Color textColorSubdued, 0
            Locate i% + 1, dataStartColumn
            Print LTrim$(Str$(i%))
            Locate i% + 1, dataStartColumn + 7
            Print "--"
            Locate i% + 1, dataStartColumn + 16
            Print "--"
        End If
    Next i%
End Sub

Sub printFightInventory (interfaceStartColumn As Integer, retreatsRemaining As Integer)
    Color textColorSubdued, 0
    Locate 1, interfaceStartColumn
    Print "Bandages:"
    Locate 1, interfaceStartColumn + 10
    Print Using "##"; heroInventory.bandageCount
    Locate 1, interfaceStartColumn + 14
    Print "Ammo:"
    Locate 1, interfaceStartColumn + 20
    Print Using "###"; heroInventory.ammoCount
    Locate 1, interfaceStartColumn + 25
    Print "HP:"
    Locate 1, interfaceStartColumn + 30
    Print Using "##"; heroHP
    Locate 2, interfaceStartColumn
    Print "Retreats remaining:"
    Locate 2, interfaceStartColumn + 20
    Print Using "##"; retreatsRemaining
End Sub

Sub clearFightInterface (interfaceLines() As String * 39)
    For i% = LBound(interfaceLines) To UBound(interfaceLines)
        interfaceLines(i%) = ""
    Next i%
End Sub

Sub printToFightInterface (interfaceStartColumn As Integer, interfaceLines() As String * 39, newLine As String * 39)
    Color textColor, 0
    For i% = LBound(interfaceLines) To UBound(interfaceLines) - 1
        interfaceLines(i%) = interfaceLines(i% + 1)
        Locate i%, interfaceStartColumn
        Print interfaceLines(i%)
    Next i%
    interfaceLines(UBound(interfaceLines)) = newLine
    Locate UBound(interfaceLines), interfaceStartColumn
    Print interfaceLines(UBound(interfaceLines))
End Sub

Function isTraversible (neighborRow As Integer, neighborColumn As Integer)
    result% = 1
    For i% = LBound(roomList) To UBound(roomList)
        if neighborRow = roomList(i%).roomRow _
            and roomList(i%).roomColumn <= neighborColumn and neighborColumn < roomList(i%).roomColumn + roomList(i%).roomWidth _
            and roomList(i%).roomColumn + roomList(i%).doorColumn <> neighborColumn then
            result% = 0
        End If
        if neighborRow = roomList(i%).roomRow + roomList(i%).roomHeight - 1 _
            and roomList(i%).roomColumn <= neighborColumn and neighborColumn < roomList(i%).roomColumn + roomList(i%).roomWidth _
            and roomList(i%).roomColumn + roomList(i%).doorColumn <> neighborColumn then
            result% = 0
        End If
        if neighborColumn = roomList(i%).roomColumn _
            and roomList(i%).roomRow <= neighborRow and neighborRow < roomList(i%).roomRow + roomList(i%).roomHeight _
            and roomList(i%).roomRow + roomList(i%).doorRow <> neighborRow then
            result% = 0
        End If
        if neighborColumn = roomList(i%).roomColumn + roomList(i%).roomWidth - 1 _
            and roomList(i%).roomRow <= neighborRow and neighborRow < roomList(i%).roomRow + roomList(i%).roomHeight _
            and roomList(i%).roomRow + roomList(i%).doorRow <> neighborRow then
            result% = 0
        End If
    Next i%
    isTraversible = result%
End Function

Function canMove (currentZombie As zombie, nextRow As Integer, nextColumn As Integer)
    if isOnScreenNew(nextRow, nextColumn) = 1 _
        and currentZombie.zombieRow - zombieAggro <= nextRow and nextRow <= currentZombie.zombieRow + zombieAggro _
        and currentZombie.zombieColumn - zombieAggro <= nextColumn and nextColumn <= currentZombie.zombieColumn + zombieAggro _
        and nextRow >= worldRowMin and nextColumn >= worldColumnMin then
        If isTraversible(nextRow, nextColumn) = 1 Then
            canMove = 1
        Else
            canMove = 0
        End If
    Else
        canMove = 0
    End If
End Function

Sub handleMessageClearing
    stepsSinceLastMessage = stepsSinceLastMessage + 1
    If stepsSinceLastMessage = 4 Then
        Locate 1, 1
        Print clearText
    End If
End Sub

Sub pickUpItems
    For i% = 1 To UBound(partList)
        If partList(i%).isPartFound = 0 And partList(i%).partRow = heroRow And partList(i%).partColumn = heroColumn Then
            partList(i%).isPartFound = 1
            heroInventory.partCount = heroInventory.partCount + 1
            If heroInventory.partCount = partCount Then
                Call printMessage("You found all the parts. Return to the teleporter.")
            Else
                Call printMessage("You found a part.")
            End If
            doRedraw = 1
        End If
    Next i%
    For i% = 1 To UBound(bandageList)
        If bandageList(i%).isBandageFound = 0 And bandageList(i%).bandageRow = heroRow And bandageList(i%).bandageColumn = heroColumn Then
            bandageList(i%).isBandageFound = 1
            heroInventory.bandageCount = heroInventory.bandageCount + 1
            Call printMessage("You found a bandage.")
            doRedraw = 1
        End If
    Next i%
    For i% = 1 To UBound(ammoList)
        If ammoList(i%).isAmmoFound = 0 And ammoList(i%).ammoRow = heroRow And ammoList(i%).ammoColumn = heroColumn Then
            ammoList(i%).isAmmoFound = 1
            thisAmmoAmount = Int(Rnd * (ammoMax - ammoMin + 1)) + ammoMin
            heroInventory.ammoCount = heroInventory.ammoCount + thisAmmoAmount
            Call printMessage("You found " + Str$(thisAmmoAmount) + " rounds of ammo.")
            doRedraw = 1
        End If
    Next i%
End Sub

Sub printMessage (message As String)
    Locate 1, 1
    Print clearText
    Color textColor, 0
    Locate 1, 1
    Print message
    stepsSinceLastMessage = 0
End Sub

Sub showHelp
    Cls
    Color textColor, 0
    Print " After the zombie invasion, you worked tirelessly to evacuate the island with"
    Print " your teleporter. Unfortunately, the teleporter broke just as the last person"
    Print " departed, leaving you behind alone. You must find the parts to repair the"
    Print " teleporter and escape the zombie-invested island with just a rifle and"
    Print " a knife for protection."
    Locate 7, 2
    Print "OBJECTS"
    Color heroColor, 0
    Locate 8, 2
    Print Chr$(heroChar)
    Color textColor, 0
    Locate 8, 4
    Print "you, the hero"
    Color partColor, 0
    Locate 9, 2
    Print Chr$(partChar)
    Color textColor, 0
    Locate 9, 4
    Print "a part needed to fix teleporter"
    Color teleporterColor, 0
    Locate 10, 2
    Print Chr$(teleporterChar)
    Color textColor, 0
    Locate 10, 4
    Print "the teleporter needed to escape"
    Color zombieColor, 0
    Locate 11, 2
    Print Chr$(zombieChar)
    Color textColor, 0
    Locate 11, 4
    Print "a pack of zombies"
    Color bandageColor, 0
    Locate 12, 2
    Print Chr$(bandageChar)
    Color textColor, 0
    Locate 12, 4
    Print "a bandage to heal your wounds"
    Color ammoColor, 0
    Locate 13, 2
    Print Chr$(ammoChar)
    Color textColor, 0
    Locate 13, 4
    Print "ammo for your rifle"
    Color roomColor, 0
    Locate 14, 2
    Print Chr$(doorChar)
    Color textColor, 0
    Locate 14, 4
    Print "building doors"
    Color roomColor, 0
    Locate 15, 2
    Print Chr$(cornerULChar) + " " + Chr$(cornerURChar) + " " + Chr$(cornerLLChar) + " " + Chr$(cornerLRChar) + " " + Chr$(wallHChar) + " " + Chr$(wallVChar)
    Color textColor, 0
    Locate 15, 14
    Print "building walls"
    Locate 7, 40
    Print "COMMANDS"
    Locate 8, 40
    Print "H - show this help screen"
    Locate 9, 40
    Print "arrow keys - move"
    Locate 10, 40
    Print "B - use bandage"
    Locate 11, 40
    Print "Q - quit"
    Locate screenRowMax, 1
    Print "Press any key to continue."
End Sub










