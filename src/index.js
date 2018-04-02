import './main.css';
import { Main } from './Main.elm';
import { FirebaseConnector } from './FirebaseConnector.elm';
import registerServiceWorker from './registerServiceWorker';
import fbConfig from '../firebase-config';

firebase.initializeApp(fbConfig);

var app = Main.embed(document.getElementById('root'));

app.ports.fetchWorkouts.subscribe(function(weekNumber) {
  const workouts = firebase.database().ref('workouts');
  workouts.on('value', (snapshot) => {
    const workoutsForWeek = snapshot.val().filter(wo => wo.weekNumber === weekNumber);
    app.ports.receiveWorkouts.send(workoutsForWeek);
  });
})

registerServiceWorker();
