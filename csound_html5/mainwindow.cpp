#include <cstring>
#include <QFileDialog>
#include <QScrollBar>
#include <QTextStream>
#include "mainwindow.h"
#include "ui_mainwindow.h"

extern "C" int argdecode(CSOUND *csound, int size, char **argv);

static QString license = R"(
# Csound with HTML5

## License

This software is licensed under the terms of the GNU Lesser General Public License, which is the same license as used for Csound.

## Introduction

CHSound is a bare-bones "front end" for editing and performing Csound pieces:

1. Csound pieces written using standard CSD files. Use the Play and Stop buttons on the menu bar to control the Csound performance.

2. Csound pieces written using standard CSD files with an <html> element that contains an embedded HTML5 page. Csound appears as a "csound" object in the JavaScript context of this Web page. Use the Play and Stop buttons on the menu bar to control the Csound performance.

3. Csound pieces written as HTML files. Csound appears as a "csound" object in the JavaScript context of this Web page. Do not use the Play and Stop buttons on the menu bar, you must use JavaScript code and/or HTML controls on the Web page with JavaScript event handlers to control the Csound performance.


)";

void messageCallback(CSOUND *csound, int level, const char *format, va_list valist)
{
    (void) level;
    auto hostdata = csoundGetHostData(csound);
    MainWindow *mainWindow = (MainWindow *)hostdata;
    char buffer[0x1002];
    vsnprintf(buffer, 0x1000, format, valist);
    // Using signal and slot makes this thread-safe.
    QString qbuffer = buffer;
    emit mainWindow->updateMessages(qbuffer);
}

void scatterArgs(const std::string buffer,
                 std::vector<std::string> &args)
{
    std::string separators = " \t\n\r";
    args.clear();
    size_t first = 0;
    size_t last = 0;
    for(;;) {
        first = buffer.find_first_not_of(separators, last);
        if (first == std::string::npos) {
            return;
        }
        last = buffer.find_first_of(separators, first);
        if (last == std::string::npos) {
            args.push_back(buffer.substr(first));
            return;
        } else {
            args.push_back(buffer.substr(first, last - first));
        }
    }
}

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    settings("Irreducible Productions", "CHSound"),
    stop(true),
    finished(true),
    thread(nullptr)
{
    csound.setHostData(this);
    csound.setMessageCallback(&messageCallback);
    ui->setupUi(this);
    ui->htmlTab->page()->setWebChannel(&channel);
    channel.registerObject("csound", &csound);
    ui->manualTab->setUrl(QUrl("http://csound.github.io/docs/manual/indexframes.html"));
    ui->portalView->setUrl(QUrl("http://csound.github.io/"));
    ui->licenseEdit->setPlainText(license);
    ui->csdEdit->createStandardContextMenu();
}

MainWindow::~MainWindow()
{
    csound.stop();
    //delete ui;
}

void MainWindow::on_updateMessages(const QString &line)
{
    ui->messagesEdit->moveCursor (QTextCursor::End);
    ui->messagesEdit->insertPlainText (line);
    ui->messagesEdit->moveCursor (QTextCursor::End);
}

void MainWindow::newCsd()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    auto csd = R"(<CsoundSynthesizer>
<CsOptions>
-odac
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 100
nchnls = 2
0dbfs = 1.0

</CsInstruments>
<CsScore>

</CsScore>
</CsoundSynthesizer>
)";
    ui->csdEdit->clear();
    ui->csdEdit->appendPlainText(csd);
    ui->csdEdit->moveCursor (QTextCursor::Start);
}

/**
 * Returns the whole tag and its contents.
 */
QString getElement(const QString &text, const QString &tag)
{
    QString element = text.section("<" + tag, 1, 1);
    element = element.section("</" + tag + ">", 0, 0);
    return "<" + tag + element + "</" + tag + ">";
}


// https://bugreresize(ui->tabs->currentWidget()->size());ports.qt.io/browse/QTBUG-53411 in Qt SDK 5.7.0 means that
// if a page with a QWebChannel is reloaded, the qt module vanishes and
// the channel quits working. I also find problems with tabbing back to a tab with a browser
// which is supposed to show something, but doesn't. As a workaround, we remove and recreate the
// browser whenever we show a new tab or load a new page. This is not necessary with Qt 5.8.

void MainWindow::replaceBrowser(int which)
{
    if (which == 1) {
        auto index = ui->tabs->indexOf(ui->htmlTab);
        ui->tabs->removeWidget(ui->htmlTab);
        delete ui->htmlTab;
        ui->htmlTab = new CsoundWebView();
        ui->tabs->insertWidget(index, ui->htmlTab);
    } else if (which == 2) {
        auto index = ui->tabs->indexOf(ui->manualTab);
        ui->tabs->removeWidget(ui->manualTab);
        delete ui->manualTab;
        ui->manualTab = new CsoundWebView();
        ui->tabs->insertWidget(index, ui->manualTab);
        QUrl url("http://csound.github.io/docs/manual/indexframes.html");
        ui->manualTab->setUrl(url);
    } else if (which == 3) {
        ui->portalTab->layout()->removeWidget(ui->portalView);
        delete ui->portalView;
        ui->portalView = new CsoundWebView();
        ui->portalTab->layout()->addWidget(ui->portalView);
        ui->portalView->setUrl(QUrl("http://csound.github.io/"));
    }
}

void MainWindow::saveAndLoadHtml()
{
    qDebug() << "CHSound: " << __FUNCTION__;
#if (QT_VERSION < 0x050800)
    replaceBrowser(1);
#endif
    auto text = ui->csdEdit->toPlainText();
    QFile csdfile(filename);
    csdfile.open(QIODevice::WriteOnly | QIODevice::Text);
    QTextStream out(&csdfile);
    out << text;
    csdfile.close();
    auto html = getElement(text, "html");
    // Inject necessary code to load qtwebchannel/qwebchannel.js.
    QString injection = R"(
<script type="text/javascript" src="qrc:///qtwebchannel/qwebchannel.js"></script>
<script type="text/javascript">
"use strict";
document.addEventListener("DOMContentLoaded", function() {
    try {
        console.log("Initializing Csound...");
        window.channel = new QWebChannel(qt.webChannelTransport, function(channel) {
        window.csound = channel.objects.csound;
            console.log("Initialized Csound.")
            csound.message("Initialized csound.\n");
        });
    } catch (e) {
        alert("initialize_csound error: " + e.message);
        console.log(e.message);
}
});
</script>
)";
    // Tricky because now HTML doesn't have to have a <head> element,
    // and both <html> and <head> can have attributes. So we need to find an
    // injection point that is the very first place allowed to put a <script>
    // element.
    int injection_index = html.indexOf("<head", 0, Qt::CaseInsensitive);
    if (injection_index != -1) {
        injection_index = html.indexOf(">", injection_index) + 1;
    } else {
        injection_index = html.indexOf("<html", 0, Qt::CaseInsensitive);
        injection_index = html.indexOf(">", injection_index) + 1;
    }
    html = html.insert(injection_index, injection);
    csound.message("Injected WebChannel proxy startup code into HTML page.\n");
    if (html.size() > 0) {
        ui->tabs->setCurrentIndex(1);
        QString htmlfilename = filename + ".html";
        QFile htmlfile(htmlfilename);
        htmlfile.open(QIODevice::WriteOnly | QIODevice::Text);
        QTextStream out(&htmlfile);
        out << html;
        htmlfile.close();
        ui->htmlTab->page()->setWebChannel(&channel);
        channel.registerObject("csound", &csound);
        csound.message("Injected WebChannel 'csound' proxy into HTML page.\n");
        ui->htmlTab->setUrl(QUrl::fromLocalFile(htmlfile.fileName()));
        ui->htmlTab->layout()->invalidate();
        csound.message("Loaded HTML page.\n");
    } else {
        ui->htmlTab->load(QUrl("about:blank"));
        ui->tabs->setCurrentIndex(0);
    }
    repaint();
}

void MainWindow::openCsd()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    filename = QFileDialog::getOpenFileName(this, tr("Open file"), "", tr("Csound files (*.csd *.orc *.sco);;HTML files (*.htm, *.html);;All files (*.*)"));
    if (filename.size() > 0) {
        QFile file(filename);
        if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
            return;
        }
        QString text = file.readAll().toStdString().c_str();
        file.close();
        ui->csdEdit->clear();
        ui->csdEdit->setPlainText(text);
        ui->csdEdit->moveCursor (QTextCursor::Start);
        if (text.indexOf("</html>", 0, Qt::CaseInsensitive) != -1){
           saveAndLoadHtml();
        } else {
           showCsdTab();
        }
        setWindowTitle(filename);
        setWindowFilePath(filename);
        this->statusBar()->showMessage("Loaded " + filename);
    }
}

void MainWindow::saveCsd()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    QFile file(filename);
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
        return;
    }
    QString text = ui->csdEdit->toPlainText();
    QTextStream stream(&file);
    stream << text;
    file.close();
    setWindowTitle(filename);
    setWindowFilePath(filename);
    this->statusBar()->showMessage("Saved as " + filename);
}

void MainWindow::saveCsdAs()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    auto newfilename = QFileDialog::getSaveFileName(this, tr("Open file"), "", tr("Csound files (*.csd *.orc *.sco)"));
    if (newfilename.size() > 0) {
        filename = newfilename;
    }
    saveCsd();
}

void MainWindow::run(const QString &csd_)
{
    qDebug() << "CHSound: " << __FUNCTION__;
    int result = 0;
    emit updateStatus("Csound is compiling...");
    result = csound.compileCsdText(csd_.toStdString().c_str());
    result = csound.start();
    emit updateStatus("Csound is running...");
    for (stop = false, finished = false;
         ((stop == false) && (finished == false)); )
    {
        finished = csound.performKsmps();
    }
    emit updateStatus("Csound has stopped.");
    result = csound.cleanup();
    if (result) {
        emit updateStatus("Failed to clean up Csound performance.");
    }
    csound.reset();
}

void MainWindow::runFile()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    saveCsd();
    if (ui->csdEdit->toPlainText().indexOf("</html>", 0, Qt::CaseInsensitive) != -1) {
        saveAndLoadHtml();
    } else {
        showCsdTab();
    }
    if (filename.endsWith(".csd", Qt::CaseInsensitive)) {
        runCsdText(ui->csdEdit->toPlainText());
    }
}

void MainWindow::runCsdText(const QString &csdText)
{
    qDebug() << "CHSound: " << __FUNCTION__;
    if (thread != nullptr) {
        stop = true;
        thread->join();
        delete thread;
        thread = nullptr;
    }
    ui->messagesEdit->clear();
    thread = new std::thread(&MainWindow::run, this, csdText);
}

void MainWindow::stopCsd()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    stop = true;
    if (thread != 0) {
        thread->join();
        delete thread;
        thread = nullptr;
    }
}

void MainWindow::on_backButton_clicked()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    ui->portalView->back();
}

void MainWindow::on_loadButton_clicked()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    QUrl url(ui->urlEdit->text());
    ui->portalView->load(url);
}

void MainWindow::on_csoundHomeButton_clicked()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    QUrl url("http://csound.github.io/");
    ui->portalView->load(url);
}

void MainWindow::on_forwardButton_clicked()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    ui->portalView->forward();
}

void MainWindow::on_stopLoadingButton_clicked()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    ui->portalView->stop();
}

void MainWindow::on_googleButton_clicked()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    QUrl url("http://google.com/");
    ui->portalView->load(url);
    ui->portalView->setFocus();
}

void MainWindow::on_urlEdit_returnPressed()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    QUrl url(ui->urlEdit->text());
    ui->portalView->load(url);
    ui->portalView->setFocus();
}

void MainWindow::makeFullScreen()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    if (this->isFullScreen()) {
        showNormal();
    } else {
        showFullScreen();
    }
}

void MainWindow::showCsdTab()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    ui->tabs->setCurrentIndex(0);
}

void MainWindow::showHtmlTab()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    saveAndLoadHtml();
    ui->tabs->setCurrentIndex(1);
}

void MainWindow::showManualTab()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    replaceBrowser(2);
    ui->tabs->setCurrentIndex(2);
    ui->manualTab->updateGeometry();
}

void MainWindow::showPortalTab()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    replaceBrowser(3);
    ui->tabs->setCurrentIndex(3);
    ui->portalView->updateGeometry();
    ui->portalTab->update();
}

void MainWindow::showLicenseTab()
{
    qDebug() << "CHSound: " << __FUNCTION__;
    ui->tabs->setCurrentIndex(4);
}
