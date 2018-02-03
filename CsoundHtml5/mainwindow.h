#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QLabel>
#include <QMainWindow>
#include <QSettings>
#include <thread>
#include "csoundwebview.h"
#include "qcsound.h"
#include "finddialog.h"
#include "findreplacedialog.h"

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
    QString csound_messages_buffer;
    FindDialog *find_dialog;
    FindReplaceDialog *find_replace_dialog;
    QLabel *cursorLabel;
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();
    void run(const QString &csdtext);
    CSOUND *getCsound()
    {
        return csound.getCsound();
    }
    bool isPlaying() const
    {
        return ((stop == false) && (finished == false));
    }
    void replaceBrowser(int which);
    void acceptFullScreen(QWebEngineFullScreenRequest request);
public slots:
    void loadFile(const QString &filepath);
    void newCsd();
    void openFile();
    void saveFile();
    void saveAndLoadHtml();
    void saveFileAs();
    /**
     * @brief runFile -- Run the contents of the editor. CSD files are run
     * using Csound in the calling process, HTML is run using the
     * QtWebEngineView in its own process. HTML-only files must control Csound
     * using only JavaScript in the HTML code.
     */
    void runFile();
    void stopCsd();
    void runCsdText(const QString &csd);
    void makeFullScreen();
    void showCsdTab();
    void showHtmlTab();
    void showDebugTab();
    void showManualTab();
    void showPortalTab();
    void showLicenseTab();
    void findDialog();
    void findReplaceDialog();
    void readSettings();
    void writeSettings();
    void closeEvent(QCloseEvent *event);
    void showContextMenu(const QPoint &point);
    void on_backButton_clicked();
    void on_loadButton_clicked();
    void on_csoundHomeButton_clicked();
    void on_forwardButton_clicked();
    void on_stopLoadingButton_clicked();
    void on_googleButton_clicked();
    void on_urlEdit_returnPressed();
    void on_updateMessages(const QString &);
    void on_cursorPositionChanged();
signals:
    void updateMessages(const QString &line);
    void updateStatus(const QString &message);
protected:
    void createActions();
    void contextMenuEvent(QContextMenuEvent *event);
};

#endif // MAINWINDOWCLASS_H
