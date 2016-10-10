#include "qcsound.h"
#include <QDebug>

QCsound::QCsound(QObject *parent) : QObject(parent), thread_(nullptr) {
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
    return Get0dBFS();
}

Q_INVOKABLE int QCsound::getApiVersion() {
    return GetAPIVersion();
}

Q_INVOKABLE double QCsound::getControlChannel(const QString &name) {
    return GetChannel(name.toLocal8Bit());
}

Q_INVOKABLE int64_t QCsound::getCurrentTimeSamples() {
    return GetCurrentTimeSamples();
}

Q_INVOKABLE QString QCsound::getEnv(const QString &name) {
    return GetEnv(name.toLocal8Bit());
}

Q_INVOKABLE int QCsound::getKsmps() {
    return GetKsmps();
}

Q_INVOKABLE int QCsound::getNchnls() {
    return GetNchnls();
}

Q_INVOKABLE int QCsound::getNchnlsInput() {
    return GetNchnlsInput();
}

Q_INVOKABLE QString QCsound::getOutputName() {
    return GetOutputName();
}

Q_INVOKABLE double QCsound::getScoreOffsetSeconds() {
    return GetScoreOffsetSeconds();
}

Q_INVOKABLE double QCsound::getScoreTime() {
    return GetScoreTime();
}

Q_INVOKABLE int QCsound::getSr() {
    return GetSr();
}

Q_INVOKABLE QString QCsound::getStringChannel(const QString &name) {
    char buffer[0x100];
    GetStringChannel(name.toLocal8Bit(), buffer);
    return buffer;
}

Q_INVOKABLE int QCsound::getVersion() {
    return GetVersion();
}

Q_INVOKABLE bool QCsound::isPlaying() {
    return ((stop_ == false) && (finished == false));
}

Q_INVOKABLE int QCsound::isScorePending() {
    return IsScorePending();
}

Q_INVOKABLE void QCsound::message(const QString &text) {
    Message(text.toLocal8Bit());
}

Q_INVOKABLE int QCsound::perform() {
    stop();
    thread_ = new std::thread(&QCsound::perform_thread_routine, this);
    if (thread_) {
        return 0;
    } else {
        return 1;
    }
}

Q_INVOKABLE int QCsound::perform_thread_routine() {
    qDebug() << __FUNCTION__;
    int result = 0;
    result = Start();
    message("Csound is running...");
    for (stop_ = false, finished = false;
         ((stop_ == false) && (finished == false)); )
    {
        finished = PerformKsmps();
    }
    message("Csound has stopped.");
    result = Cleanup();
    if (result) {
        message("Failed to clean up Csound performance.");
    }
    Reset();
    return result;
}

Q_INVOKABLE int QCsound::readScore(const QString &text) {
   return ReadScore(text.toLocal8Bit());
}

Q_INVOKABLE void QCsound::rewindScore() {
    RewindScore();
}

void QCsound::run(const QString &csd_)
{
    qDebug() << __FUNCTION__;
    int result = 0;
    emit updateStatus("Csound is compiling...");
    result = CompileCsdText(csd_.toStdString().c_str());
    result = Start();
    emit updateStatus("Csound is running...");
    for (stop_ = false, finished = false;
         ((stop_ == false) && (finished == false)); )
    {
        finished = PerformKsmps();
    }
    emit updateStatus("Csound has stopped.");
    result = Cleanup();
    if (result) {
        emit updateStatus("Failed to clean up Csound performance.");
    }
    Reset();
}

Q_INVOKABLE int QCsound::runUtility(const QString &command, int argc, char **argv) {
    return RunUtility(command.toLocal8Bit(), argc, argv);
}

Q_INVOKABLE int QCsound::scoreEvent(char type, const double *pFields, long numFields) {
    return ScoreEvent(type, pFields, numFields);
}

Q_INVOKABLE void QCsound::setControlChannel(const QString &name, double value) {
    SetChannel(name.toLocal8Bit(), value);
}

Q_INVOKABLE int QCsound::setGlobalEnv(const QString &name, const QString &value) {
    return SetGlobalEnv(name.toLocal8Bit(), value.toLocal8Bit());
}

Q_INVOKABLE void QCsound::setInput(const QString &name){
    SetInput(name.toLocal8Bit());
}

Q_INVOKABLE int QCsound::setOption(const QString &name){
    return SetOption((char *)name.toStdString().c_str());
}

Q_INVOKABLE void QCsound::setOutput(const QString &name, const QString &type, const QString &format){
    SetOutput((char *)name.toStdString().c_str(), (char *)type.toStdString().c_str(), (char *)format.toStdString().c_str());
}

Q_INVOKABLE void QCsound::setScoreOffsetSeconds(double value){
    SetScoreOffsetSeconds(value);
}

Q_INVOKABLE void QCsound::setScorePending(bool value){
    SetScorePending(value);
}

Q_INVOKABLE void QCsound::setStringChannel(const QString &name, const QString &value){
    SetChannel(name.toStdString().c_str(), (char *)value.toStdString().c_str());
}

Q_INVOKABLE double QCsound::tableGet(int table_number, int index){
    return TableGet(table_number, index);
}

Q_INVOKABLE int QCsound::tableLength(int table_number){
    return TableLength(table_number);
}

Q_INVOKABLE void QCsound::tableSet(int table_number, int index, double value){
    TableSet(table_number, index, value);
}

Q_INVOKABLE void QCsound::stop(){
    stop_ = true;
    if (thread_ != nullptr) {
        thread_->join();
        delete thread_;
        thread_ = nullptr;
    }
}



