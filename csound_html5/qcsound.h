#ifndef CSOUNDPROXY_H
#define CSOUNDPROXY_H

#include <QObject>
#include <csound.hpp>

/**
 * Methods of the Csound API exposed to JavaScript are as follows.
 * Note that methods that return values do so via a completion callback.

csoundCompileCsd	compileCsd
csoundCompileCsdText	compileCsdText
csoundCompileOrc	compileOrc
//csoundCreate	create
//csoundDestroy	destroy
csoundEvalCode	evalCode
csoundGet0dBFS	get0DBFS
csoundGetAPIVersion	getAPIVersion
csoundGetControlChannel	getControlChannel
csoundGetCurrentTimeSamples	getCurrentTimeSamples
csoundGetEnv	getEnv
csoundGetKsmps	getKsmps
csoundGetNchnls	getNchnls
csoundGetNchnlsInput	getNchnlsInput
csoundGetOutputName	getOutputName
csoundGetScoreOffsetSeconds	getScoreOffsetSeconds
csoundGetScoreTime	getScoreTime
csoundGetSr	getSr
csoundGetStringChannel	getStringChannel
csoundGetVersion	getVersion
csoundIsScorePending	isScorePending
csoundMessage	message
csoundPerform	perform
csoundReadScore	readScore
csoundRewindScore	rewindScore
csoundRunUtility	runUtility
csoundScoreEvent	scoreEvent
csoundSetControlChannel	setControlChannel
csoundSetGlobalEnv	setGlobalEnv
csoundSetInput	setInput
csoundSetOption	setOption
csoundSetOutput	setOutput
csoundSetScoreOffsetSeconds	setScoreOffsetSeconds
csoundSetScorePending	setScorePending
csoundSetStringChannel	setStringChannel
csoundStop	stop
csoundTableGet	tableGet
csoundTableLength	tableLength
csoundTableSet	tableSet
    isPlaying
*/

class QCsound : public QObject, public Csound
{
    Q_OBJECT
public:
    explicit QCsound(QObject *parent = 0);
    Q_INVOKABLE int compileCsd(const QString &filename);
    Q_INVOKABLE int compileCsdText(const QString &text);
    Q_INVOKABLE int compileOrc(const QString &text);
    Q_INVOKABLE double evalCode(const QString &text);
    Q_INVOKABLE double get0dBFS();
    //Q_INVOKABLE int getApiVersion();
    //Q_INVOKABLE double getControlChannel(const QString &name);
    //Q_INVOKABLE int64_t getCurrentTimeSamples(const QString &name);
    //Q_INVOKABLE QString getEnv(const QString &name);
    //Q_INVOKABLE int getKsmps();
    //Q_INVOKABLE int getNchnls();
    //Q_INVOKABLE int getNchnlsInput();
    //Q_INVOKABLE QString getOutputName();
    //Q_INVOKABLE double getScoreOffsetSeconds();
    //Q_INVOKABLE double getScoreTime();
    //Q_INVOKABLE int getSr();
    //Q_INVOKABLE QString getStringChannel(const QString &name);
    //Q_INVOKABLE int getVersion();
    //Q_INVOKABLE int isPlaying();
    //Q_INVOKABLE int isScorePending();
    Q_INVOKABLE void message(const QString &text);
    //Q_INVOKABLE int perform();
    Q_INVOKABLE int readScore(const QString &text);
    //Q_INVOKABLE int rewindScore();
    //Q_INVOKABLE int runUtility(const QString &command);
    //Q_INVOKABLE int scoreEvent(char type, const double *pFields, long numFields);
    Q_INVOKABLE void setControlChannel(const QString &name, double value);
    //Q_INVOKABLE int setGlobalEnv(const QString &name, const QString &value);
    //Q_INVOKABLE int setInput(const QString &name);
    //Q_INVOKABLE int setOption(const QString &name);
    //Q_INVOKABLE int setOutput(const QString &name);
    //Q_INVOKABLE int setScoreOffsetSeconds(double value);
    //Q_INVOKABLE int setScorePending(bool value);
    //Q_INVOKABLE void setStringChannel(const QString &name, const QString &value);
    //Q_INVOKABLE double tableGet(int table_number, int index);
    //Q_INVOKABLE int tableLength(int table_number);
    //Q_INVOKABLE int tableSet(int table_number, int index, double value);
    //Q_INVOKABLE int stop();
signals:
    void stateChanged(int state);
public slots:
};

#endif // CSOUNDPROXY_H
