#include <QDeclarativeContext>

#include "hsqml.h"
#include "HsQMLManager.h"
#include "HsQMLWindow.h"

HsQMLWindow::HsQMLWindow(QObject *globalObject, const QUrl &src)
  : QMainWindow()
  , mSource(src)
{
  setCentralWidget(&mView);
  mView.setResizeMode(QDeclarativeView::SizeRootObjectToView);
  mView.setAttribute(Qt::WA_OpaquePaintEvent);
  mView.setAttribute(Qt::WA_NoSystemBackground);
  mView.setFocus();
  mView.show();

  // Don't delete on close
  setAttribute(Qt::WA_DeleteOnClose, false);

  // Setup context
  mView.rootContext()->setContextProperty("window", this);
  mView.rootContext()->setContextObject(globalObject);

  // Setup for QML performance
  mView.setOptimizationFlags(QGraphicsView::DontSavePainterState);
  mView.setViewportUpdateMode(QGraphicsView::BoundingRectViewportUpdate);
  mView.scene()->setItemIndexMethod(QGraphicsScene::NoIndex);

  // Setup for QML key handling
  mView.viewport()->setFocusPolicy(Qt::NoFocus);
  mView.setFocusPolicy(Qt::StrongFocus);
  mView.scene()->setStickyFocus(true);

  // Set the source last, after we have set up the window and the
  // context (since the src is going to reference elements in the
  // context).
  mView.setSource(src);

  show();
}

HsQMLWindow::~HsQMLWindow()
{
}

QUrl HsQMLWindow::source() const
{
  return mSource;
}

QDeclarativeEngine* HsQMLWindow::engine() const
{
  return mView.engine();
}

void HsQMLWindow::setSource(const QUrl& url)
{
  mSource = url;
  mView.setSource(url);
}
