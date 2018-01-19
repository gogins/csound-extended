/********************************************************************************
** Form generated from reading UI file 'mainwindow.ui'
**
** Created by: Qt User Interface Compiler version 5.8.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_MAINWINDOW_H
#define UI_MAINWINDOW_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QPlainTextEdit>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSplitter>
#include <QtWidgets/QStackedWidget>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QToolBar>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>
#include "csoundwebview.h"

QT_BEGIN_NAMESPACE

class Ui_MainWindow
{
public:
    QAction *actionNew;
    QAction *actionOpen;
    QAction *actionSave;
    QAction *actionSave_as;
    QAction *actionRun;
    QAction *actionStop;
    QAction *actionMakeFullscreen;
    QAction *actionShowCsdTab;
    QAction *actionShowHTMLTab;
    QAction *actionShowManualTab;
    QAction *actionShowPortalTab;
    QAction *actionShowLicenseTab;
    QAction *actionDebug;
    QWidget *centralWidget;
    QVBoxLayout *verticalLayout_2;
    QSplitter *splitter;
    QStackedWidget *tabs;
    QWidget *csdTab;
    QVBoxLayout *verticalLayout_7;
    QPlainTextEdit *csdEdit;
    CsoundWebView *htmlTab;
    QVBoxLayout *verticalLayout;
    CsoundWebView *manualTab;
    QVBoxLayout *verticalLayout_3;
    QWidget *portalTab;
    QVBoxLayout *verticalLayout_4;
    QWidget *browserToolsWidget;
    QHBoxLayout *horizontalLayout;
    QPushButton *forwardButton;
    QPushButton *backButton;
    QPushButton *stopLoadingButton;
    QPushButton *loadButton;
    QLabel *label;
    QLineEdit *urlEdit;
    QPushButton *googleButton;
    QPushButton *csoundHomeButton;
    CsoundWebView *portalView;
    QWidget *licenseTab;
    QVBoxLayout *verticalLayout_9;
    QPlainTextEdit *licenseEdit;
    QPlainTextEdit *messagesEdit;
    QToolBar *toolBar;
    QStatusBar *statusBar;

    void setupUi(QMainWindow *MainWindow)
    {
        if (MainWindow->objectName().isEmpty())
            MainWindow->setObjectName(QStringLiteral("MainWindow"));
        MainWindow->resize(979, 619);
        QFont font;
        font.setFamily(QStringLiteral("Courier New"));
        font.setPointSize(8);
        font.setBold(false);
        font.setItalic(false);
        font.setWeight(50);
        MainWindow->setFont(font);
        QIcon icon;
        icon.addFile(QStringLiteral("cs.ico"), QSize(), QIcon::Normal, QIcon::Off);
        MainWindow->setWindowIcon(icon);
        MainWindow->setStyleSheet(QStringLiteral("font: 8pt \"Courier New\";"));
        MainWindow->setUnifiedTitleAndToolBarOnMac(true);
        actionNew = new QAction(MainWindow);
        actionNew->setObjectName(QStringLiteral("actionNew"));
        actionOpen = new QAction(MainWindow);
        actionOpen->setObjectName(QStringLiteral("actionOpen"));
        actionSave = new QAction(MainWindow);
        actionSave->setObjectName(QStringLiteral("actionSave"));
        actionSave_as = new QAction(MainWindow);
        actionSave_as->setObjectName(QStringLiteral("actionSave_as"));
        actionRun = new QAction(MainWindow);
        actionRun->setObjectName(QStringLiteral("actionRun"));
        actionStop = new QAction(MainWindow);
        actionStop->setObjectName(QStringLiteral("actionStop"));
        actionMakeFullscreen = new QAction(MainWindow);
        actionMakeFullscreen->setObjectName(QStringLiteral("actionMakeFullscreen"));
        actionShowCsdTab = new QAction(MainWindow);
        actionShowCsdTab->setObjectName(QStringLiteral("actionShowCsdTab"));
        actionShowHTMLTab = new QAction(MainWindow);
        actionShowHTMLTab->setObjectName(QStringLiteral("actionShowHTMLTab"));
        actionShowManualTab = new QAction(MainWindow);
        actionShowManualTab->setObjectName(QStringLiteral("actionShowManualTab"));
        actionShowPortalTab = new QAction(MainWindow);
        actionShowPortalTab->setObjectName(QStringLiteral("actionShowPortalTab"));
        actionShowLicenseTab = new QAction(MainWindow);
        actionShowLicenseTab->setObjectName(QStringLiteral("actionShowLicenseTab"));
        actionDebug = new QAction(MainWindow);
        actionDebug->setObjectName(QStringLiteral("actionDebug"));
        centralWidget = new QWidget(MainWindow);
        centralWidget->setObjectName(QStringLiteral("centralWidget"));
        verticalLayout_2 = new QVBoxLayout(centralWidget);
        verticalLayout_2->setSpacing(6);
        verticalLayout_2->setContentsMargins(11, 11, 11, 11);
        verticalLayout_2->setObjectName(QStringLiteral("verticalLayout_2"));
        verticalLayout_2->setContentsMargins(4, 4, 4, 4);
        splitter = new QSplitter(centralWidget);
        splitter->setObjectName(QStringLiteral("splitter"));
        splitter->setOrientation(Qt::Vertical);
        tabs = new QStackedWidget(splitter);
        tabs->setObjectName(QStringLiteral("tabs"));
        tabs->setStyleSheet(QStringLiteral("background-color:lightgray"));
        csdTab = new QWidget();
        csdTab->setObjectName(QStringLiteral("csdTab"));
        verticalLayout_7 = new QVBoxLayout(csdTab);
        verticalLayout_7->setSpacing(6);
        verticalLayout_7->setContentsMargins(11, 11, 11, 11);
        verticalLayout_7->setObjectName(QStringLiteral("verticalLayout_7"));
        verticalLayout_7->setContentsMargins(0, 0, 0, 0);
        csdEdit = new QPlainTextEdit(csdTab);
        csdEdit->setObjectName(QStringLiteral("csdEdit"));
        csdEdit->setFont(font);
        csdEdit->setStyleSheet(QLatin1String("background-color: rgb(255, 255, 255);\n"
"color: rgb(0, 0, 0);"));

        verticalLayout_7->addWidget(csdEdit);

        tabs->addWidget(csdTab);
        htmlTab = new CsoundWebView();
        htmlTab->setObjectName(QStringLiteral("htmlTab"));
        verticalLayout = new QVBoxLayout(htmlTab);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QStringLiteral("verticalLayout"));
        verticalLayout->setContentsMargins(0, 0, 0, 0);
        tabs->addWidget(htmlTab);
        manualTab = new CsoundWebView();
        manualTab->setObjectName(QStringLiteral("manualTab"));
        verticalLayout_3 = new QVBoxLayout(manualTab);
        verticalLayout_3->setSpacing(6);
        verticalLayout_3->setContentsMargins(11, 11, 11, 11);
        verticalLayout_3->setObjectName(QStringLiteral("verticalLayout_3"));
        tabs->addWidget(manualTab);
        portalTab = new QWidget();
        portalTab->setObjectName(QStringLiteral("portalTab"));
        verticalLayout_4 = new QVBoxLayout(portalTab);
        verticalLayout_4->setSpacing(6);
        verticalLayout_4->setContentsMargins(11, 11, 11, 11);
        verticalLayout_4->setObjectName(QStringLiteral("verticalLayout_4"));
        browserToolsWidget = new QWidget(portalTab);
        browserToolsWidget->setObjectName(QStringLiteral("browserToolsWidget"));
        browserToolsWidget->setMinimumSize(QSize(0, 24));
        browserToolsWidget->setMaximumSize(QSize(16777215, 24));
        horizontalLayout = new QHBoxLayout(browserToolsWidget);
        horizontalLayout->setSpacing(2);
        horizontalLayout->setContentsMargins(11, 11, 11, 11);
        horizontalLayout->setObjectName(QStringLiteral("horizontalLayout"));
        horizontalLayout->setContentsMargins(2, 2, 2, 2);
        forwardButton = new QPushButton(browserToolsWidget);
        forwardButton->setObjectName(QStringLiteral("forwardButton"));

        horizontalLayout->addWidget(forwardButton);

        backButton = new QPushButton(browserToolsWidget);
        backButton->setObjectName(QStringLiteral("backButton"));

        horizontalLayout->addWidget(backButton);

        stopLoadingButton = new QPushButton(browserToolsWidget);
        stopLoadingButton->setObjectName(QStringLiteral("stopLoadingButton"));

        horizontalLayout->addWidget(stopLoadingButton);

        loadButton = new QPushButton(browserToolsWidget);
        loadButton->setObjectName(QStringLiteral("loadButton"));

        horizontalLayout->addWidget(loadButton);

        label = new QLabel(browserToolsWidget);
        label->setObjectName(QStringLiteral("label"));

        horizontalLayout->addWidget(label);

        urlEdit = new QLineEdit(browserToolsWidget);
        urlEdit->setObjectName(QStringLiteral("urlEdit"));
        urlEdit->setStyleSheet(QStringLiteral("background-color:white;color:black"));

        horizontalLayout->addWidget(urlEdit);

        googleButton = new QPushButton(browserToolsWidget);
        googleButton->setObjectName(QStringLiteral("googleButton"));

        horizontalLayout->addWidget(googleButton);

        csoundHomeButton = new QPushButton(browserToolsWidget);
        csoundHomeButton->setObjectName(QStringLiteral("csoundHomeButton"));

        horizontalLayout->addWidget(csoundHomeButton);

        backButton->raise();
        forwardButton->raise();
        label->raise();
        urlEdit->raise();
        loadButton->raise();
        csoundHomeButton->raise();
        stopLoadingButton->raise();
        googleButton->raise();

        verticalLayout_4->addWidget(browserToolsWidget);

        portalView = new CsoundWebView(portalTab);
        portalView->setObjectName(QStringLiteral("portalView"));
        portalView->setFocusPolicy(Qt::StrongFocus);

        verticalLayout_4->addWidget(portalView);

        tabs->addWidget(portalTab);
        licenseTab = new QWidget();
        licenseTab->setObjectName(QStringLiteral("licenseTab"));
        verticalLayout_9 = new QVBoxLayout(licenseTab);
        verticalLayout_9->setSpacing(6);
        verticalLayout_9->setContentsMargins(11, 11, 11, 11);
        verticalLayout_9->setObjectName(QStringLiteral("verticalLayout_9"));
        licenseEdit = new QPlainTextEdit(licenseTab);
        licenseEdit->setObjectName(QStringLiteral("licenseEdit"));
        licenseEdit->setFrameShape(QFrame::Panel);

        verticalLayout_9->addWidget(licenseEdit);

        tabs->addWidget(licenseTab);
        splitter->addWidget(tabs);
        messagesEdit = new QPlainTextEdit(splitter);
        messagesEdit->setObjectName(QStringLiteral("messagesEdit"));
        messagesEdit->setStyleSheet(QLatin1String("background-color: rgb(0, 0, 0);\n"
"color: rgb(0, 255, 0);"));
        messagesEdit->setFrameShape(QFrame::Panel);
        messagesEdit->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
        splitter->addWidget(messagesEdit);

        verticalLayout_2->addWidget(splitter);

        MainWindow->setCentralWidget(centralWidget);
        toolBar = new QToolBar(MainWindow);
        toolBar->setObjectName(QStringLiteral("toolBar"));
        MainWindow->addToolBar(Qt::TopToolBarArea, toolBar);
        statusBar = new QStatusBar(MainWindow);
        statusBar->setObjectName(QStringLiteral("statusBar"));
        MainWindow->setStatusBar(statusBar);

        toolBar->addAction(actionNew);
        toolBar->addAction(actionOpen);
        toolBar->addAction(actionSave);
        toolBar->addAction(actionSave_as);
        toolBar->addAction(actionRun);
        toolBar->addAction(actionStop);
        toolBar->addAction(actionMakeFullscreen);
        toolBar->addSeparator();
        toolBar->addAction(actionShowCsdTab);
        toolBar->addAction(actionShowHTMLTab);
        toolBar->addAction(actionDebug);
        toolBar->addAction(actionShowManualTab);
        toolBar->addAction(actionShowPortalTab);
        toolBar->addAction(actionShowLicenseTab);

        retranslateUi(MainWindow);
        QObject::connect(actionNew, SIGNAL(triggered()), MainWindow, SLOT(newCsd()));
        QObject::connect(actionRun, SIGNAL(triggered()), MainWindow, SLOT(runFile()));
        QObject::connect(actionStop, SIGNAL(triggered()), MainWindow, SLOT(stopCsd()));
        QObject::connect(actionShowCsdTab, SIGNAL(triggered()), MainWindow, SLOT(showCsdTab()));
        QObject::connect(actionShowHTMLTab, SIGNAL(triggered()), MainWindow, SLOT(showHtmlTab()));
        QObject::connect(actionShowManualTab, SIGNAL(triggered()), MainWindow, SLOT(showManualTab()));
        QObject::connect(actionShowPortalTab, SIGNAL(triggered()), MainWindow, SLOT(showPortalTab()));
        QObject::connect(actionShowLicenseTab, SIGNAL(triggered()), MainWindow, SLOT(showLicenseTab()));
        QObject::connect(actionMakeFullscreen, SIGNAL(triggered()), MainWindow, SLOT(makeFullScreen()));
        QObject::connect(MainWindow, SIGNAL(updateMessages(QString)), MainWindow, SLOT(on_updateMessages(QString)));
        QObject::connect(actionSave, SIGNAL(triggered()), MainWindow, SLOT(saveFile()));
        QObject::connect(actionSave_as, SIGNAL(triggered()), MainWindow, SLOT(saveFileAs()));
        QObject::connect(actionOpen, SIGNAL(triggered()), MainWindow, SLOT(openFile()));
        QObject::connect(actionDebug, SIGNAL(triggered()), MainWindow, SLOT(showDebugTab()));

        QMetaObject::connectSlotsByName(MainWindow);
    } // setupUi

    void retranslateUi(QMainWindow *MainWindow)
    {
        MainWindow->setWindowTitle(QApplication::translate("MainWindow", "Csound", Q_NULLPTR));
        actionNew->setText(QApplication::translate("MainWindow", "New", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        actionNew->setToolTip(QApplication::translate("MainWindow", "Create new CSD file.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        actionNew->setShortcut(QApplication::translate("MainWindow", "Ctrl+N", Q_NULLPTR));
        actionOpen->setText(QApplication::translate("MainWindow", "Open...", Q_NULLPTR));
        actionOpen->setShortcut(QApplication::translate("MainWindow", "Ctrl+O", Q_NULLPTR));
        actionSave->setText(QApplication::translate("MainWindow", "Save", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        actionSave->setToolTip(QApplication::translate("MainWindow", "Save this CSD file.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        actionSave->setShortcut(QApplication::translate("MainWindow", "Ctrl+S", Q_NULLPTR));
        actionSave_as->setText(QApplication::translate("MainWindow", "Save as...", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        actionSave_as->setToolTip(QApplication::translate("MainWindow", "Save this CSD file with a new filename.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        actionSave_as->setShortcut(QApplication::translate("MainWindow", "Ctrl+A", Q_NULLPTR));
        actionRun->setText(QApplication::translate("MainWindow", "Play", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        actionRun->setToolTip(QApplication::translate("MainWindow", "Play this CSD file in Csound.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        actionRun->setShortcut(QApplication::translate("MainWindow", "F5", Q_NULLPTR));
        actionStop->setText(QApplication::translate("MainWindow", "Stop", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        actionStop->setToolTip(QApplication::translate("MainWindow", "Stop running Csound.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        actionStop->setShortcut(QApplication::translate("MainWindow", "F6", Q_NULLPTR));
        actionMakeFullscreen->setText(QApplication::translate("MainWindow", "Fullscreen", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        actionMakeFullscreen->setToolTip(QApplication::translate("MainWindow", "Make the main window full screen (or not).", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        actionMakeFullscreen->setShortcut(QApplication::translate("MainWindow", "F11", Q_NULLPTR));
        actionShowCsdTab->setText(QApplication::translate("MainWindow", "Editor", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        actionShowCsdTab->setToolTip(QApplication::translate("MainWindow", "Show the code editor.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        actionShowCsdTab->setShortcut(QApplication::translate("MainWindow", "Ctrl+1", Q_NULLPTR));
        actionShowHTMLTab->setText(QApplication::translate("MainWindow", "HTML", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        actionShowHTMLTab->setToolTip(QApplication::translate("MainWindow", "Show the HTML page.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        actionShowHTMLTab->setShortcut(QApplication::translate("MainWindow", "Ctrl+2", Q_NULLPTR));
        actionShowManualTab->setText(QApplication::translate("MainWindow", "Manual", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        actionShowManualTab->setToolTip(QApplication::translate("MainWindow", "Go to the Csound Reference Manual.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        actionShowManualTab->setShortcut(QApplication::translate("MainWindow", "Ctrl+3", Q_NULLPTR));
        actionShowPortalTab->setText(QApplication::translate("MainWindow", "Portal", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        actionShowPortalTab->setToolTip(QApplication::translate("MainWindow", "Go to the Csound home page.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        actionShowPortalTab->setShortcut(QApplication::translate("MainWindow", "Ctrl+4", Q_NULLPTR));
        actionShowLicenseTab->setText(QApplication::translate("MainWindow", "About", Q_NULLPTR));
        actionShowLicenseTab->setIconText(QApplication::translate("MainWindow", "About", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        actionShowLicenseTab->setToolTip(QApplication::translate("MainWindow", "Shows the Csound license and describes this program.", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        actionShowLicenseTab->setShortcut(QApplication::translate("MainWindow", "Ctrl+5", Q_NULLPTR));
        actionDebug->setText(QApplication::translate("MainWindow", "Debug", Q_NULLPTR));
#ifndef QT_NO_TOOLTIP
        actionDebug->setToolTip(QApplication::translate("MainWindow", "Show JavaScript DevTools...", Q_NULLPTR));
#endif // QT_NO_TOOLTIP
        actionDebug->setShortcut(QApplication::translate("MainWindow", "Ctrl+D", Q_NULLPTR));
        forwardButton->setText(QApplication::translate("MainWindow", "Forward", Q_NULLPTR));
        backButton->setText(QApplication::translate("MainWindow", "Back", Q_NULLPTR));
        stopLoadingButton->setText(QApplication::translate("MainWindow", "Stop", Q_NULLPTR));
        loadButton->setText(QApplication::translate("MainWindow", "Load", Q_NULLPTR));
        label->setText(QApplication::translate("MainWindow", " URL:", Q_NULLPTR));
        urlEdit->setText(QApplication::translate("MainWindow", "http://csound.github.io/", Q_NULLPTR));
        urlEdit->setPlaceholderText(QString());
        googleButton->setText(QApplication::translate("MainWindow", "Google", Q_NULLPTR));
        csoundHomeButton->setText(QApplication::translate("MainWindow", "Csound home", Q_NULLPTR));
        toolBar->setWindowTitle(QApplication::translate("MainWindow", "toolBar", Q_NULLPTR));
    } // retranslateUi

};

namespace Ui {
    class MainWindow: public Ui_MainWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MAINWINDOW_H
