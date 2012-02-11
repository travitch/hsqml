#include <HsFFI.h>
#include <QObject>
#include <qdeclarative.h>
#include <QDeclarativeEngine>
#include <QDeclarativeListProperty>
#include <qdeclarativeprivate.h>

#include "HsQMLObject.h"
#include "HsQMLClass.h"

HsQMLObject::HsQMLObject(void* haskell, HsQMLClass* klass)
  : mHaskell(haskell)
  , mKlass(klass)
{
  mKlass->ref();
}

HsQMLObject::~HsQMLObject()
{
  hs_free_stable_ptr((HsStablePtr)mHaskell);
  mKlass->deref();
}

const QMetaObject* HsQMLObject::metaObject() const
{
  return QObject::d_ptr->metaObject ?
    QObject::d_ptr->metaObject : &mKlass->mMetaObject;
}

void* HsQMLObject::qt_metacast(const char* clname)
{
  if (!clname) {
    return 0;
  }
  if (!strcmp(clname,
        mKlass->mMetaObject.d.stringdata +
        mKlass->mMetaObject.d.data[MD_CLASS_NAME])) {
      return static_cast<void*>(const_cast<HsQMLObject*>(this));
  }
  return QObject::qt_metacast(clname);
}

int HsQMLObject::qt_metacall(QMetaObject::Call c, int id, void** a)
{
  id = QObject::qt_metacall(c, id, a);
  if (id < 0) {
    return id;
  }
  if (QMetaObject::InvokeMetaMethod == c) {
    // We handle methods a bit differently than standard Qt.  When
    // dispatching a method, subtract out the number of signals from
    // the id we are passed.  Qt stores both signals and methods in
    // its methods array, but we don't store signals (they are
    // completely separate and are not dispatched through the meta
    // object system).
    mKlass->methods()[id - mKlass->mSignalCount](this, a);
    id -= mKlass->mMethodCount;
  }
  else if (QMetaObject::ReadProperty == c) {
    mKlass->properties()[2*id](this, a);
    id -= mKlass->mPropertyCount;
  }
  else if (QMetaObject::WriteProperty == c) {
    HsQMLUniformFunc uf = mKlass->properties()[2*id+1];
    if (uf) {
      char** args =(char**)a;
      uf(this, a);
    }
    id -= mKlass->mPropertyCount;
  }
  else if (QMetaObject::QueryPropertyDesignable == c ||
           QMetaObject::QueryPropertyScriptable == c ||
           QMetaObject::QueryPropertyStored == c ||
           QMetaObject::QueryPropertyEditable == c ||
           QMetaObject::QueryPropertyUser == c) {
    id -= mKlass->mPropertyCount;
  }
  return id;
}

void* HsQMLObject::haskell() const
{
  return mHaskell;
}

extern "C" HsQMLObjectHandle* hsqml_create_object(
  void* haskell, HsQMLClassHandle* kHndl)
{
  HsQMLObject* obj = new HsQMLObject(haskell, (HsQMLClass*)kHndl);
  QDeclarativeEngine::setObjectOwnership(
    obj, QDeclarativeEngine::JavaScriptOwnership);
  return (HsQMLObjectHandle*)obj;
}

extern void* hsqml_get_haskell(HsQMLObjectHandle* hndl)
{
  HsQMLObject* obj = (HsQMLObject*)hndl;
  return  obj->haskell();
}

extern "C" void hsqml_allocate_in_place(void *memory, void *priv, HsQMLClassHandle *khdl)
{
  HsQMLClass *klass = (HsQMLClass*)khdl;
  new (memory) HsQMLObject(priv, klass);
  HsQMLObject *o = (HsQMLObject*)memory;
}

extern "C" void hsqml_register_type(HsQMLPlacementFunc placementAllocator,
    const char *uri, int versionMajor, int versionMinor, const char *qmlName)
{
  QDeclarativePrivate::RegisterType rt;
  rt.version = 0;

  // Not supporting QDeclarativeItems; there are not drawing
  // primitives exposed in this library currently, so there isn't much
  // point.

  rt.typeId = qMetaTypeId<QObject*>();
  rt.listId = qMetaTypeId<QDeclarativeListProperty<QObject> >();
  rt.attachedPropertiesFunction =
    QDeclarativePrivate::attachedPropertiesFunc<QObject>();
  rt.attachedPropertiesMetaObject =
    QDeclarativePrivate::attachedPropertiesMetaObject<QObject>();
  rt.parserStatusCast =
    QDeclarativePrivate::StaticCastSelector<QObject, QDeclarativeParserStatus>::cast();
  rt.valueSourceCast =
    QDeclarativePrivate::StaticCastSelector<QObject, QDeclarativePropertyValueSource>::cast();
  rt.valueInterceptorCast =
    QDeclarativePrivate::StaticCastSelector<QObject, QDeclarativePropertyValueInterceptor>::cast();

  rt.objectSize = sizeof(HsQMLObject);
  rt.create = placementAllocator;
  rt.uri = uri;
  rt.versionMajor = versionMajor;
  rt.versionMinor = versionMinor;
  rt.elementName = qmlName;
  rt.metaObject = &HsQMLObject::staticMetaObject;

  rt.extensionObjectCreate = 0;
  rt.extensionMetaObject = 0;
  rt.customParser = 0;

  QDeclarativePrivate::qmlregister(QDeclarativePrivate::TypeRegistration, &rt);
}

extern "C" void hsqml_emit_signal(void* hndl, int signum,
    void *args)
{
  HsQMLObject* obj = (HsQMLObject*)hndl;
  QMetaObject::activate(obj, &HsQMLObject::staticMetaObject, signum, (void**)args);
}
