#include "qcsound.h"

QCsound::QCsound(QObject *parent) : QObject(parent) {
}

Q_INVOKABLE int QCsound::compileCsd(const QString &filename) {
    return CompileCsd(filename.toLocal8Bit());
}

Q_INVOKABLE int QCsound::compileCsdText(const QString &text) {
    return CompileCsdText(text.toLocal8Bit());
}

Q_INVOKABLE int QCsound::compileOrc(const QString &text) {
    return CompileOrc(text.toLocal8Bit());
}

Q_INVOKABLE double QCsound::evalCode(const QString &text) {
    return EvalCode(text.toLocal8Bit());
}

Q_INVOKABLE double QCsound::get0dBFS() {
    return this->Get0dBFS();
}

Q_INVOKABLE void QCsound::message(const QString &text) {
    Message(text.toLocal8Bit());
}

Q_INVOKABLE int QCsound::readScore(const QString &text) {
   return ReadScore(text.toLocal8Bit());
}

Q_INVOKABLE void QCsound::setControlChannel(const QString &name, double value) {
    SetChannel(name.toLocal8Bit(), value);
}

