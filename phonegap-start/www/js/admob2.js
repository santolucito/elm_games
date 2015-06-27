/*var admobid = {};
if( /(android)/i.test(navigator.userAgent) ) { 
    admobid = { // for Android
        banner: 'ca-app-pub-1481128684796488/8510497853',
        interstitial: 'ca-app-pub-6869992474017983/1657046752'
    };
} else if(/(ipod|iphone|ipad)/i.test(navigator.userAgent)) {
    admobid = { // for iOS
        banner: 'ca-app-pub-6869992474017983/4806197152',
        interstitial: 'ca-app-pub-6869992474017983/7563979554'
    };
} else {
    admobid = { // for Windows Phone
        banner: 'ca-app-pub-6869992474017983/8878394753',
        interstitial: 'ca-app-pub-6869992474017983/1355127956'
    };
}

*/


//document.addEventListener('deviceready', initApp, false);

function initApp() {
  admob.initAdmob("ca-app-pub-1481128684796488/8510497853","");
  admob.showBanner(admob.BannerSize.BANNER,admob.Position.TOP_APP);
/*    if (AdMob) {
        AdMob.createBanner({
            adId : admobid.banner,
            position : AdMob.AD_POSITION.BOTTOM_CENTER,
            autoShow : true
        });
    }
*/
}

