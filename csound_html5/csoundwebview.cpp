#include "csoundwebview.h"
#include <QCoreApplication>
#include <QString>
#include "message_event.h"

const QString CsoundWebView::kUrlBlank = "about:blank";

CsoundWebView::CsoundWebView(QWidget* parent) :
    QWebEngineView(parent) {
    qDebug() << __FUNCTION__ << QThread::currentThreadId();
}

CsoundWebView::~CsoundWebView() {
    qDebug() << __FUNCTION__;
}

QVariant CsoundWebView::evaluateJavaScript(const QString& scriptSource) {
    qDebug() << __FUNCTION__ << QThread::currentThreadId();
    auto page_ = page();
    if (page_) {
        page_->runJavaScript(scriptSource);
        return true;
    } else {
        return false;
    }
}

bool CsoundWebView::acceptNavigationRequest(const QUrl &url, QWebEnginePage::NavigationType type, bool isMainFrame)
{
    qDebug() << __FUNCTION__ << QThread::currentThreadId() << url << type << isMainFrame;
    return true;
}

void CsoundWebView::resizeEvent(QResizeEvent* e) {
    qDebug() << __FUNCTION__ << QThread::currentThreadId();
    e->accept();
}

void CsoundWebView::closeEvent(QCloseEvent* e) {
    qDebug() << __FUNCTION__ << QThread::currentThreadId();
    e->accept();
}

void CsoundWebView::showEvent(QShowEvent* e) {
    qDebug() << __FUNCTION__ << url();
    adjustSize();
    repaint();
    e->accept();
}

void CsoundWebView::OnMessageEvent(MessageEvent* e) {
    qDebug() << __FUNCTION__ << QThread::currentThreadId();
    // Cross thread. Not in ui thread here.
    QCoreApplication::postEvent(this, e, Qt::HighEventPriority);
    e->accept();
}


void CsoundWebView::customEvent(QEvent* e) {
    qDebug() << __FUNCTION__ << QThread::currentThreadId();
    if (e->type() == MessageEvent::MessageEventType) {
        MessageEvent* event = static_cast<MessageEvent*>(e);
        QString name = event->name();
        QVariantList args = event->args();
        qDebug() << __FUNCTION__ << name << args;
        emit jsMessage(name, args);
    }
}

void CsoundWebView::OnAddressChange(const QString& url) {
    qDebug() << __FUNCTION__ << url;
    emit urlChanged(QUrl(url));
}

void CsoundWebView::OnTitleChange(const QString& title) {
    qDebug() << __FUNCTION__ << title;
    emit titleChanged(title);
}

void CsoundWebView::SetLoading(bool isLoading) {
    qDebug() << __FUNCTION__ << isLoading << url();
    if (isLoading) {
        emit loadStarted();
    } else {
        emit loadFinished(true);
    }
}

void CsoundWebView::SetNavState(bool canGoBack, bool canGoForward) {
    qDebug() << __FUNCTION__ << canGoBack << canGoForward;
    emit navStateChanged(canGoBack, canGoForward);
}

void CsoundWebView::OnAfterCreated() {
    qDebug() << __FUNCTION__;
    resize(size());
}


