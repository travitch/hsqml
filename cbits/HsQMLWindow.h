#ifndef HSQML_WINDOW_H
#define HSQML_WINDOW_H

#include <QMainWindow>
// #include <QDeclarativeContext>
// #include <QDeclarativeItem>
// #include <QGraphicsScene>
#include <QDeclarativeView>
#include <QUrl>

#include "HsQMLManager.h"

//class QDeclarativeComponent;

class HsQMLWindow : public QMainWindow
{
    Q_OBJECT

public:
  HsQMLWindow(QObject*, const QUrl&);
  virtual ~HsQMLWindow();
  QUrl source() const;
  void setSource(const QUrl&);
  Q_PROPERTY(QUrl source READ source WRITE setSource);

  QDeclarativeEngine* engine() const;
  Q_PROPERTY(QDeclarativeEngine* engine READ engine);

private:
//  HsQMLEngine* mEngine;
//  QDeclarativeContext mContext;
  QDeclarativeView mView;
  QUrl mSource;
//  QDeclarativeComponent* mComponent;
//  QSize mInitialSize;
};

#endif /*HSQML_WINDOW_H*/
