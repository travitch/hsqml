
#include <QMetaType>
#include <QDeclarativeContext>

#include "HsQMLManager.h"
#include "HsQMLWindow.h"

static QMutex gMutex;
static HsQMLManager* gManager;

static void HsQMLInitImpl()
{
  gMutex.lock();
  if (!gManager) {
    int* argcp = new int[1];
    *argcp = 1;
    char** argv = new char*[1];
    argv[0] = new char[1];
    argv[0][0] = '\0';
    gManager = new HsQMLManager(*argcp, argv);
  }
  gMutex.unlock();
}

static void HsQMLInit()
{
  if (!gManager) {
    HsQMLInitImpl();
  }
}

HsQMLManager::HsQMLManager(int& argc, char** argv)
  : mApp(argc, argv)
{
}

HsQMLManager::~HsQMLManager()
{
}

void HsQMLManager::run()
{
    mApp.exec();
}

void HsQMLManager::createEngine(QObject *globalObject, const QUrl &url)
{
  mWindows.push_back(new HsQMLWindow(globalObject, url));
}

extern "C" void hsqml_run()
{
  HsQMLInit();
  gManager->run();
}

extern "C" void hsqml_create_engine(void* globalObject, const char* initialURL)
{
  QUrl url = QUrl(QString(initialURL));

  HsQMLInit();
  gManager->createEngine((QObject*)globalObject, url);
}
