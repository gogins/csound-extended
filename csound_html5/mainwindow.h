#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QSettings>
#include <thread>
#include "csoundwebview.h"
#include "qcsound.h"

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT
public:
    Ui::MainWindow *ui;
    QCsound csound;
    QWebChannel channel;
    QSettings settings;
    QString filename;
    bool stop;
    bool finished;
    std::thread *thread;
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();
    void run(const QString &csdtext);
    CSOUND *getCsound()
    {
        return csound.GetCsound();
    }
    bool isPlaying() const
    {
        return ((stop == false) && (finished == false));
    }
    void replaceBrowser(int which);
public slots:
    void newCsd();
    void openCsd();
    void saveCsd();
    void saveAndLoadHtml();
    void saveCsdAs();
    void runCsd();
    void stopCsd();
    void sendOrc();
    void sendSco();
    void runCsdText(const QString &csd);
    void makeFullScreen();
    void showCsdTab();
    void showHtmlTab();
    void showManualTab();
    void showPortalTab();
    void showLicenseTab();
signals:
    void updateMessages(const QString &line);
    void updateStatus(const QString &message);
private slots:
    void on_backButton_clicked();
    void on_loadButton_clicked();
    void on_csoundHomeButton_clicked();
    void on_forwardButton_clicked();
    void on_stopLoadingButton_clicked();
    void on_googleButton_clicked();
    void on_urlEdit_returnPressed();
    void on_updateMessages(const QString &);
};

#endif // MAINWINDOWCLASS_H
