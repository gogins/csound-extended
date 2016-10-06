#include "message_event.h"

const QEvent::Type MessageEvent::MessageEventType = (QEvent::Type)QEvent::registerEventType();

MessageEvent::MessageEvent(const QString& name, const QVariantList& args)
    : QEvent(MessageEventType),
      name_(name),
      args_(args) {
}

MessageEvent::~MessageEvent(void) {
}

QString MessageEvent::name() const {
  return name_;
}

QVariantList MessageEvent::args() const {
  return args_;
}
