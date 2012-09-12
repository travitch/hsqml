import QtQuick 1.0

TabWidget  {
  id: tabs
  width: 640; height: 480
//  anchors.fill: parent

  Rectangle  {
    property string title:"Red -- " + hsTypeStatic
    anchors.fill: parent
    color: "#e3e3e3"

    Rectangle  {
      anchors.fill: parent; anchors.margins: 20
      color: "#ff7f7f"
      Text  {
        width: parent.width - 20
        anchors.centerIn: parent; horizontalAlignment: Qt.AlignHCenter
        text: "Roses are red"
        font.pixelSize: 20
        wrapMode: Text.WordWrap
      }
      ClickNode {
        anchors.centerIn: parent;
        color: 'blue';
        operation: "node"
      }
      signal doNotify(int i)
      Component.onCompleted: {
        doNotify.connect(hsShowInt)
        doNotify(parent.width)
        doNotify(parent.height)
        contentChanged.connect(hsChanged)
      }
// hsShowInt(parent.width)
    }
  }

  Rectangle  {
    property string title: "Green"
    anchors.fill: parent
    color: "#e3e3e3"

    Rectangle  {
      anchors.fill: parent; anchors.margins: 20
      color: "#7fff7f"
      Text  {
        width: parent.width - 20
        anchors.centerIn: parent; horizontalAlignment: Qt.AlignHCenter
        text: "Flower stems are green"
        font.pixelSize: 20
        wrapMode: Text.WordWrap
      }
    }
  }

  Rectangle  {
    property string title: "Blue"
    anchors.fill: parent; color: "#e3e3e3"

    Rectangle  {
      anchors.fill: parent; anchors.margins: 20
      color: "#7f7fff"
      Text  {
        width: parent.width - 20
        anchors.centerIn: parent; horizontalAlignment: Qt.AlignHCenter
        text: "Violets are blue"
        font.pixelSize: 20
        wrapMode: Text.WordWrap
      }
    }
  }
}