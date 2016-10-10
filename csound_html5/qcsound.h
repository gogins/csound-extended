#ifndef CSOUNDPROXY_H
#define CSOUNDPROXY_H

#include <QObject>
#include <csound.hpp>
#include <thread>

/**
 * Note that Q_INVOKABLE methods that return values via a QWebChannel,
 * do so via a completion callback that must be passed as an additional
 * parameter to the method.
 */

class QCsound : public QObject, public Csound
{
    Q_OBJECT
    bool stop_;
    bool finished;
    std::thread *thread_;
public:
    explicit QCsound(QObject *parent = 0);
    Q_INVOKABLE int compileCsd(const QString &filename);
    Q_INVOKABLE int compileCsdText(const QString &text);
    Q_INVOKABLE int compileOrc(const QString &text);
    Q_INVOKABLE double evalCode(const QString &text);
    Q_INVOKABLE double get0dBFS();
    Q_INVOKABLE int getApiVersion();
    Q_INVOKABLE double getControlChannel(const QString &name);
    Q_INVOKABLE int64_t getCurrentTimeSamples();
    Q_INVOKABLE QString getEnv(const QString &name);
    Q_INVOKABLE int getKsmps();
    Q_INVOKABLE int getNchnls();
    Q_INVOKABLE int getNchnlsInput();
    Q_INVOKABLE QString getOutputName();
    Q_INVOKABLE double getScoreOffsetSeconds();
    Q_INVOKABLE double getScoreTime();
    Q_INVOKABLE int getSr();
    Q_INVOKABLE QString getStringChannel(const QString &name);
    Q_INVOKABLE int getVersion();
    Q_INVOKABLE bool isPlaying();
    Q_INVOKABLE int isScorePending();
    Q_INVOKABLE void message(const QString &text);
    /**
     * Starts a Csound performance in a separate thread of execution, which
     * can be ended by calling stop(). Other API calls must/may then be used
     * to compile a csd, orc, or sco. Csound messages are emitted via the
     * updateMessages() signal.
     */
    Q_INVOKABLE int perform();
    Q_INVOKABLE int perform_thread_routine();
    Q_INVOKABLE int readScore(const QString &text);
    Q_INVOKABLE void rewindScore();
    /**
     * @brief run -- Compiles and performs the CSD file. Csound messages are
     * emitted via the updateMessages() signal.
     * @param csd_ Text of CSD file.
     */
    void run(const QString &csd_);
    Q_INVOKABLE int runUtility(const QString &command, int argc, char **argv);
    Q_INVOKABLE int scoreEvent(char type, const double *pFields, long numFields);
    Q_INVOKABLE void setControlChannel(const QString &name, double value);
    Q_INVOKABLE int setGlobalEnv(const QString &name, const QString &value);
    Q_INVOKABLE void setInput(const QString &name);
    Q_INVOKABLE int setOption(const QString &name);
    Q_INVOKABLE void setOutput(const QString &name, const QString &type, const QString &format);
    Q_INVOKABLE void setScoreOffsetSeconds(double value);
    Q_INVOKABLE void setScorePending(bool value);
    Q_INVOKABLE void setStringChannel(const QString &name, const QString &value);
    Q_INVOKABLE void stop();
    Q_INVOKABLE double tableGet(int table_number, int index);
    Q_INVOKABLE int tableLength(int table_number);
    Q_INVOKABLE void tableSet(int table_number, int index, double value);
signals:
    void updateMessages(const QString &line);
    void updateStatus(const QString &message);
};

#endif // CSOUNDPROXY_H
