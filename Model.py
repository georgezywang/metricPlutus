import numpy as np
import os
import torch
import torchvision
from time import time
from torchvision import transforms
from torch import nn
from torch.optim import Adam
from torch.utils.data import DataLoader, Dataset
from torch.utils.data.sampler import SubsetRandomSampler
from torch.autograd import Variable
import matplotlib.pyplot as plt
import tqdm
import pandas as pd

class PlutusData(Dataset):
    def __init__(self, csv_file, root_dir, transform):
        path = os.path.join(root_dir, csv_file)
        dat = np.loadtxt(path, delimiter = ",", dtype = np.float32, skiprows = 1)
        self.inp = torch.from_numpy(dat[:, 3:11])
        self.oup = torch.from_numpy(dat[:, 1:3])
        self.samples = dat.shape[0]
        self.transform = transform

    def __getitem__(self, index):
        return self.inp[index], self.oup[index]
    
    def __len__(self):
        return self.samples

#network Architecture
class PlutusNet(nn.Module):
    def __init__(self, input_size, hidden_size):
        super().__init__()

        self.fc1 = nn.Linear(input_size, hidden_size)
        self.b1 = nn.Sigmoid()
        self.fc2 = nn.Linear(hidden_size, hidden_size)
        self.b2 = nn.ReLU()
        self.fc3 = nn.Linear(hidden_size, hidden_size)
        self.b3 = nn.ReLU()
        self.fc4 = nn.Linear(hidden_size, 1)

    def forward(self,x):
        ex = self.fc1(x)
        rev = self.fc1(x)
        ex = self.b1(x)
        rev = self.b1(x)
        ex = self.fc2(x)
        rev = self.fc2(x)
        ex = self.b2(x)
        rev = self.b2(x)
        ex = self.fc3(x)
        rev = self.fc3(x)
        ex = self.b3(x)
        rev = self.b3(x)
        ex = self.fc4(x)
        rev = self.fc4(x)
        return torch.cat((ex, rev),1)

#load and normalize data
def constructTransform(mean, std):
    mean = (mean,)
    std = (std,)
    normalization_transforms = [torchvision.transforms.ToTensor(), 
                                torchvision.transforms.Normalize(mean, std)]
    transform = transforms.Compose(normalization_transforms)
    return transform

#train test split
def splitData(dataset):
    dataset_size = len(data_set)
    indices = list(range(dataset_size))
    dataset_size = len(data_set)
    indices = list(range(dataset_size))
    np.random.shuffle(indices)
    split = int(np.floor(test_split * dataset_size))
    train_indices, test_indices = indices[split:], indices[:split]
    return train_indices, test_indices


def get_device():
    if torch.cuda.is_available():
        device = torch.device('cuda:0')
    else:
        device = torch.device('cpu') # don't have GPU 
    return device

def train(model, x, y, optimizer, criterion):
    model.zero_grad()
    output = model(x)
    loss =criterion(output,y)
    loss.backward()
    optimizer.step()
    return loss, output

def preprocess(fileName):
    rawFile = pd.read_csv(fileName)
    print(rawFile.dtypes)
    rawFile.to_csv("data_new.csv", index = False)

transform = constructTransform(mean = 0, std = 1)
#change the directory to your own when testing, ../content is colab specific
preprocess("data.csv")
data_set = PlutusData("data_new.csv", "../content", transform = transform)
batch_size = 512
test_split = 0.2
train_indices, test_indices = splitData(PlutusData)
train_sampler = SubsetRandomSampler(train_indices)
test_sampler = SubsetRandomSampler(test_indices)
train_loader = torch.utils.data.DataLoader(data_set, batch_size = batch_size, sampler = train_sampler)
test_loader = torch.utils.data.DataLoader(data_set, batch_size = batch_size, sampler = test_sampler)

dataiter = iter(train_loader)
features, labels = dataiter.next()
print("features: ", features, "Label: ", labels)

plutusNet = PlutusNet(input_size = 8, hidden_size = 8)

criterion = nn.MSELoss()
EPOCHS = 500
optimizer = Adam(plutusNet.parameters(), lr = 0.0005)

device = get_device()

loss_tracker = []
tune = 0.000001
for epoch in range(EPOCHS):
  loss = torch.tensor([100])
  progress_bar = tqdm.notebook.tqdm(train_loader, ncols=1000)
  for i, (features, labels) in enumerate(progress_bar):
    x_train = Variable(features.view(-1, 8))
    labels = Variable(labels)
    optimizer.zero_grad()
    outputs = plutusNet(x_train)
    loss = criterion(outputs, labels)
    loss = torch.sqrt(loss + tune)
    loss.backward()
    optimizer.step()
    loss_tracker.append(loss.data)                                  

    if (i+1) % 100 == 0 or (i+1) == len(train_loader):   
      progress_bar.set_description('Epoch [%d/%d], Step [%d/%d], Val, Training Loss: %.4f'
              %(epoch+1, EPOCHS, i+1, len(train_loader), loss.data))

plt.plot(loss_tracker)
plt.ylabel("Loss")
plt.xlabel("Step Number")
plt.title("Loss over time")
plt.show()
