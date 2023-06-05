import torch
import torch.nn as nn
import torch.optim as optim

from sklearn import datasets, linear_model
import matplotlib.pyplot as plt
#import numpy as np

import matplotlib.pyplot as plt
import numpy as np
from sklearn import datasets, linear_model
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.utils import shuffle

# Load the diabetes dataset
diabetes_X, diabetes_y = datasets.load_diabetes(return_X_y=True)

# Use only one feature
diabetes_X = diabetes_X[:, np.newaxis, 2]

# Split the data into training/testing sets
diabetes_X_train = diabetes_X[:-20]
diabetes_X_test = diabetes_X[-20:]

# Split the targets into training/testing sets
diabetes_y_train = diabetes_y[:-20]
diabetes_y_test = diabetes_y[-20:]

def sck_learn():
    # Create linear regression object
    regr = linear_model.LinearRegression()

    # Train the model using the training sets
    regr.fit(diabetes_X_train, diabetes_y_train)

    # Make predictions using the testing set
    diabetes_y_pred = regr.predict(diabetes_X_test)

    # The coefficients
    print("Coefficients: \n", regr.coef_)
    # The mean squared error
    print("Mean squared error: %.2f" % mean_squared_error(diabetes_y_test, diabetes_y_pred))
    # The coefficient of determination: 1 is perfect prediction
    print("Coefficient of determination: %.2f" % r2_score(diabetes_y_test, diabetes_y_pred))

    # Plot outputs
    plt.scatter(diabetes_X_test, diabetes_y_test, color="black")
    plt.plot(diabetes_X_test, diabetes_y_pred, color="blue", linewidth=3)

    plt.xticks(())
    plt.yticks(())

    #plt.show()

class LR(nn.Module):
    def __init__(self):
        super(LR,self).__init__()
        self.linear = nn.Linear(1,1)
    def forward(self,x):
        return self.linear(x)
    
sck_learn()

diabetes_X_train = torch.Tensor(diabetes_X_train)
diabetes_X_test = torch.Tensor(diabetes_X_test)

# Split the targets into training/testing sets
diabetes_y_train = torch.Tensor(diabetes_y_train)
diabetes_y_test = torch.Tensor(diabetes_y_test)
rate_learning = 1e-2
model = LR()
optimizer = optim.Adam(model.parameters(), lr=rate_learning)
#optimizer = optim.SGD(model.parameters(),lr=rate_learning)
criterion = nn.MSELoss()
n_epoch = 100000
print_every = n_epoch/10
for epoch in range(n_epoch):
    diabetes_X_train, diabetes_y_train = shuffle(diabetes_X_train, diabetes_y_train)
    hyp = model(diabetes_X_train)
    #print(hyp)
    #cost = F.mse_loss(hyp,diabetes_y_train)
    #print(hyp.shape,diabetes_y_train.shape)
    cost = criterion(hyp.squeeze(1), diabetes_y_train)
    #print(cost)
    optimizer.zero_grad()
    cost.backward()
    optimizer.step()
    if epoch % print_every == 0:
        params = list(model.parameters())
        W = params[0].item()
        b = params[1].item()
        print(f"Epoch {epoch:4d} W: {W:.3f}, b: {b:.3f} cost: {cost:.6f}")


params = list(model.parameters())
W = params[0].item()
b = params[1].item()

print(f"Final {epoch:4d} W: {W:.3f}, b: {b:.3f} cost: {cost:.6f}")
diabetes_y_pred = W * diabetes_X_test + b

#plt.scatter(diabetes_X_test, diabetes_y_test, color="black")
plt.plot(diabetes_X_test, diabetes_y_pred, color="green", linewidth=3)

plt.xticks(())
plt.yticks(())

plt.show()
