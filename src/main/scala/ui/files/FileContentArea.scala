package ui.files

import javafx.beans.binding.Bindings
import javafx.scene.control.{ContextMenu, ListCell, ListView, MenuItem, TextField, Tooltip}
import javafx.scene.input.{KeyCode, MouseButton}
import javafx.scene.paint.Color
import javafx.scene.text.{Font, Text, TextFlow}
import model.{TextProcessor, TextWithReading}
import ui.Globals

class FileContentArea(
  onClickListener: Int => Unit,
  onSplit: (Int, String, String) => Unit,
  onMerge: Int => Unit,
  isUnpaired: Int => Boolean,
  canSplit: Boolean,
  beforeEdit: () => Unit,
  afterEdit: Int => Unit,
  onUndo: () => Unit,
  onRedo: () => Unit
) extends ListView[String] {
  setOnKeyPressed(event => {
    val modifier = event.isControlDown || event.isMetaDown
    if (modifier && event.getCode == KeyCode.Z && !event.isShiftDown) {
      onUndo(); event.consume()
    } else if (modifier && (event.getCode == KeyCode.Y ||
               (event.getCode == KeyCode.Z && event.isShiftDown))) {
      onRedo(); event.consume()
    }
  })
  setEditable(true)
  disableProperty.bind(Bindings.isEmpty(getItems))

  setCellFactory(_ => new ListCell[String] {
    private val editor = new TextField()

    private def safeSplitPoint(text: String, caret: Int): Boolean = {
      // Reject splits inside [kanji|reading] markup.
      var inside = false
      for (i <- 0 until math.min(caret, text.length)) {
        if (text.charAt(i) == '[') inside = true
        if (text.charAt(i) == ']') inside = false
      }
      !inside && caret > 0 && caret < text.length
    }

    private def splitAtCursor(): Unit = {
      if (!isEditing) return
      val original = editor.getText
      val caret = editor.getCaretPosition
      if (!safeSplitPoint(original, caret)) {
        val warning = new Tooltip("Place the cursor between words, outside [kanji|reading].")
        warning.show(editor, editor.localToScreen(0, 0).getX, editor.localToScreen(0, 0).getY)
        warning.setAutoHide(true)
        return
      }
      val index = getIndex
      val left = original.substring(0, caret).trim
      val right = original.substring(caret).trim
      cancelEdit()
      onSplit(index, left, right)
    }

    editor.setOnAction(_ => commitEdit(editor.getText))
    private def historyShortcut(event: javafx.scene.input.KeyEvent): Boolean = {
      val modifier = event.isControlDown || event.isMetaDown
      if (!modifier) false
      else if (event.getCode == KeyCode.Z && !event.isShiftDown) {
        onUndo(); event.consume(); true
      } else if (event.getCode == KeyCode.Y || (event.getCode == KeyCode.Z && event.isShiftDown)) {
        onRedo(); event.consume(); true
      } else false
    }

    editor.setOnKeyPressed(event => {
      if (historyShortcut(event)) ()
      else if (event.getCode == KeyCode.ESCAPE) cancelEdit()
      else if (event.getCode == KeyCode.ENTER && event.isShiftDown) {
        splitAtCursor()
        event.consume()
      }
    })

    val splitItem = new MenuItem("Split at cursor (Shift+Enter)")
    splitItem.setOnAction(_ => splitAtCursor())
    val mergeItem = new MenuItem("Merge with next line")
    mergeItem.setOnAction(_ => { if (!isEmpty) onMerge(getIndex) })
    if (canSplit) setContextMenu(new ContextMenu(splitItem, mergeItem))
    else setContextMenu(new ContextMenu(splitItem))

    override def startEdit(): Unit = {
      if (!isEmpty) {
        super.startEdit()
        editor.setText(getItem)
        setText(null)
        setGraphic(editor)
        editor.requestFocus()
        editor.selectAll()
      }
    }

    override def cancelEdit(): Unit = {
      super.cancelEdit()
      displayFurigana(getItem)
    }

    override def commitEdit(value: String): Unit = {
      val row = getIndex
      if (getItem != value) beforeEdit()
      super.commitEdit(value)
      displayFurigana(value)
      afterEdit(row)
      onClickListener(row)
    }

    override def updateItem(item: String, empty: Boolean): Unit = {
      super.updateItem(item, empty)
      if (empty || item == null) {
        setText(null)
        setGraphic(null)
        setStyle("")
      } else {
        setStyle(if (isUnpaired(getIndex)) "-fx-background-color: #ffb3b3;" else "")
        if (isEditing) {
          editor.setText(item)
          setGraphic(editor)
        } else displayFurigana(item)
      }
    }

    private def displayFurigana(value: String): Unit = {
      setText(null)
      if (value == null) setGraphic(null)
      else setGraphic(partitionsToTextFlow(TextProcessor.partitionLinesAndReadings(value)._2))
    }

    setOnKeyPressed(event => { historyShortcut(event) })

    setOnMouseClicked(event => {
      if (!isEmpty) {
        if (event.getButton == MouseButton.PRIMARY && event.getClickCount == 2) startEdit()
        else if (event.getButton == MouseButton.PRIMARY && event.getClickCount == 1) onClickListener(getIndex)
      }
    })
  })

  private def partitionsToTextFlow(partitions: List[TextWithReading]): TextFlow = {
    val texts = partitions.map {
      case TextWithReading(base, "") => new Text(base)
      case TextWithReading(base, reading) =>
        val text = new Text(base)
        text.setFill(Color.SLATEBLUE)
        text.setUnderline(true)
        val tip = new Tooltip(reading) { setFont(Font.font(Globals.uiFont)) }
        text.setOnMouseEntered(_ => Tooltip.install(text, tip))
        text.setOnMouseExited(_ => Tooltip.uninstall(text, tip))
        text
    }.map(t => { t.setFont(Font.font(Globals.uiFont)); t })
    new TextFlow(texts: _*)
  }
}
