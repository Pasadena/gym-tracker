import './main.css';
import { Main } from './Main.elm';
import { FirebaseConnector } from './FirebaseConnector.elm';
import registerServiceWorker from './registerServiceWorker';
import fbConfig from '../firebase-config';

firebase.initializeApp(fbConfig);

const database = firebase.database();

var app = Main.embed(document.getElementById('root'));

app.ports.fetchWorkouts.subscribe(function(weekNumber) {
  const workouts = database.ref('workouts');
  workouts.on('value', (snapshot) => {
    const workoutsForWeek = Object.values(snapshot.val()).filter(wo => wo.weekNumber === weekNumber);
    app.ports.receiveWorkouts.send(workoutsForWeek);
  });
});

app.ports.saveWorkout.subscribe(workout => {
  const workouts = database.ref('workouts');
  const savedDoc = workouts.push(workout);
  app.ports.saveSucceeds.send(savedDoc);
});

registerServiceWorker();
