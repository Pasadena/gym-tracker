import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import fbConfig from '../firebase-config';

Main.embed(document.getElementById('root'));

registerServiceWorker();

firebase.initializeApp(fbConfig);
console.log(fbConfig);
